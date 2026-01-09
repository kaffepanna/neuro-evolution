package se.randomserver.ne
import cats.* 
import cats.syntax.all.*
import cats.syntax.functor.*
import se.randomserver.ne.evolution.Evolution.{*, given}
import cats.effect.std.Random
import cats.effect.kernel.GenConcurrent
import cats.effect.Ref
import cats.effect.syntax.all.*
import se.randomserver.ne.the_game.Game.GameState
import se.randomserver.ne.genome.GenePool.{*, given}
import se.randomserver.ne.genome.GenePool
import se.randomserver.ne.evaluator.{Compiler, Runner}
import se.randomserver.ne.the_game.Game.Cell
import se.randomserver.ne.the_game.Game.IndividualState
import cats.effect.IO
import se.randomserver.ne.genome.SpeciationConfig
import se.randomserver.ne.genome.RandomRange
import se.randomserver.ne.evaluator.Runner.ActivationState
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork
import se.randomserver.ne.genome.Genome
import cats.effect.std.Queue
import se.randomserver.ne.the_game.Game
import se.randomserver.ne.the_game.Utils
import cats.mtl.Ask
import cats.mtl.Stateful
import spire.implicits.*
import se.randomserver.ne.genome.HasGenePool

object GameEvolution {
  import scala.compiletime.ops.int.*
  type VisionRadius = 2
  type VisionSqrt = VisionRadius * 2 + 1
  type Inputs = VisionSqrt * VisionSqrt
  type Outputs = 4
  val ROWS = 30
  val COLS = 30

  final case class GameEvolutionEnv(
    teams: Int,
    gameIterations: Int,
    gamesPerGeneration: Int,
    visionRadius: Int,
    grid: Vector[Vector[Game.Cell]],
    evolutionEnv: EvolutionEnv[Double, Double]
  ) {
    export evolutionEnv.*
    lazy val inputs: Int = ((2* visionRadius  + 1)**2)
    lazy val outputs: Int = Game.Action.values.size
    lazy val rows: Int = grid.size
    lazy val cols: Int = grid.headOption.map(_.size).getOrElse(0)
  }

  final case class GameEvolutionState(
    evolutionState: EvolutionState[Double, Double]
  )

  type HasGameEvolutionEnv[F[_]] = Ask[F, GameEvolutionEnv]
  type HasGameEvolutionState[F[_]] = Stateful[F, GameEvolutionState]

  given [F[_]](using AP: Applicative[F], GEE: HasGameEvolutionEnv[F]): HasEvolutionEnv[F, Double, Double] = new Ask[F, EvolutionEnv[Double, Double]] {
    override def applicative: Applicative[F] = AP
    override def ask[E2 >: EvolutionEnv[Double, Double]]: F[E2] = GEE.ask.map(_.evolutionEnv)
  }

  given [F[_]](using M: Monad[F], GES: Stateful[F, GameEvolutionState]): HasEvolutionState[F, Double, Double] = new Stateful[F, EvolutionState[Double, Double]] {
    override def monad: Monad[F] = M
    override def get: F[EvolutionState[Double, Double]] = GES.get.map(_.evolutionState)
    override def set(es: EvolutionState[Double, Double]): F[Unit] = GES.modify(gs => gs.copy(evolutionState = es))
    override def modify(f: EvolutionState[Double, Double] => EvolutionState[Double, Double]): F[Unit] =
      GES.modify(gs => gs.copy(evolutionState = f(gs.evolutionState)))
  }

  type GameEvo[F[_]] = Monad[F] & HasGameEvolutionState[F] & HasGameEvolutionEnv[F] & StatsCallback[F] & HasGenePool[F]

  def getGameEnv[F[_]: HasGameEvolutionEnv]: F[GameEvolutionEnv] = Ask[F, GameEvolutionEnv].ask

  def integrate[F[_]: Monad: HasGameEvolutionEnv: Parallel](
    n: Int,
    agents: Map[GenomeId, (SpeciesId, CompiledNetwork[Double])],
    acc: Vector[GameState],
    activationStates: Map[GenomeId, ActivationState[Double]]
  )(using GenConcurrent[F, Throwable]): F[Vector[GameState]] = n match {
    case 0 => Monad[F].pure(acc)
    case nn => for {
      env <- getGameEnv
      state = acc.last
      intents <- agents.map { case (id, (team, member)) =>
        val vision = Game.vision(state, id, env.visionRadius)
        val inputs = vision.flatten.map {
          case Cell.Individual(_,`team`) => 0.5d
          case Cell.Individual(_, _) => -1.0
          case Cell.Food => 1.0d
          case Cell.Obstacle => -0.5
          case _ => 0.0d
        }.zip(member.inputs.toVector.sorted).map(_.swap).toMap
        
        val activationState = activationStates(id)
        val nextActivationState = Runner.stepNetwork(member, inputs, 1.0, activationState)
        val intent = member.outputs.toVector.sorted.map(nextActivationState.apply).zip(Game.Action.values).maxBy(_._1)
        Monad[F].pure(id -> (intent._2, nextActivationState))
      }.toList.sequence.map(_.toMap)
      nextGameState = Game.step(state, intents.map((k, v) => k -> v._1))
      stuck = (acc :+ nextGameState).reverse.take(5).map(a => a.individuals.values.map(_.score).sum).toSet.size == 1
      res <- if (stuck) (acc :+ nextGameState).pure
             else integrate(nn - 1, agents, acc :+ nextGameState, intents.map((k, v) => k -> v._2))
      
    } yield res
  }

  def gameStep[F[_]: Monad: Applicative: Parallel: HasGameEvolutionEnv](count: Int = 1, members: Vector[(Int, CompiledNetwork[Double])])(using RR: RandomRange[F, Double], R: Random[F])(using GenConcurrent[F, Throwable]): F[(Vector[GameState], Map[GenomeId, Double])] = for {
    gameEnv <- getGameEnv
    shuffled <- Random[F].shuffleVector(members.toVector)
    teams = Utils.splitEvenly(shuffled, gameEnv.teams).zipWithIndex.flatMap {
      case (a, team) => a.map {
        case (id, m) => (id, (team, m))
      }
    }.toMap
    
    initialIndividuals <- Monad[F].pure(teams.toVector.map { case id -> (teamId, member) => id -> teamId })

    initialGameState = GameState.random(gameEnv.grid, initialIndividuals.toSet)
    initailActivationStates = Map.empty[GenomeId, ActivationState[Double]].withDefault { i =>
      val (_, compiled) = teams(i)
      val maxNode = compiled.blocks.flatMap(_.nodes.map(_.id)).max
      ActivationState.zero[Double](maxNode + 1)
    }
    states <- integrate(gameEnv.gameIterations, teams, Vector(initialGameState), initailActivationStates)
    
    updatedFitness = teams.map { case (id, (_, _)) =>
      id -> states.last.individuals(id).score
    }.toMap
  } yield (states, updatedFitness)

  def evaluateFitness[F[_]: Monad: Parallel: Random: HasGameEvolutionEnv: HasGameEvolutionState](using RandomRange[F, Double], GenConcurrent[F, Throwable]): F[Vector[Vector[GameState]]] = for {
    env <- getGameEnv
    state <- getState
    
    members = state.population.map { (id, genome) =>
          val compiled = Compiler.compileGenome(genome, env.transfer)
          id -> compiled
    }.toVector
    
     
    runs <- (1 to env.gamesPerGeneration).inclusive
        .toVector
        .map(n => gameStep(n, members)).sequence

    updatedFitness = runs.map(_._2).foldLeft(Map.empty[Int, (Double, Int)]) { (acc, map) =>
      map.foldLeft(acc) { case (innerAcc, (k, v)) =>
        val (sum, count) = innerAcc.getOrElse(k, (0.0, 0))
        innerAcc.updated(k, (sum + v, count + 1))
      }  
    }.map { case (k, (v, n)) => k -> v / n }
    
    _ <- setState(state.copy(fitness = updatedFitness))
  } yield runs.map(_._1)

  def pushStats[F[_]: Monad: HasGameEvolutionState: StatsCallback](runs: Vector[Vector[GameState]]): F[Unit] = for {
    state <- getState
    scores = state.species.map { species =>
        val fitnessSum = species.members.map(state.fitness).foldLeft(0d)(_ + _)
        species.id -> fitnessSum / species.members.size
    }.toMap
    game = runs.maxBy(_.size)
    stats = Stats(state.generation, game, scores)
    _ <- StatsCallback[F].pushGeneration(stats)
  } yield ()


  def step[F[_]: Monad: Parallel: HasGameEvolutionEnv: HasGameEvolutionState: HasGenePool: StatsCallback](using R: Random[F], RR: RandomRange[F, Double])(using GenConcurrent[F, Throwable]): F[Vector[GameState]] = for {
    env <- getGameEnv
    runs <- evaluateFitness
    _ <- adjustFitnessSharing[F, Double, Double]
    _ <- pushStats(runs)
    _ <- debug[F, Double, Double]
    _ <- cullSpecies[F, Double, Double]
    offspringPlan <- allocateOffspringPerSpecies[F, Double,Double]
    _ = { println(offspringPlan)}
    newPop <- reproduce[F, Double, Double](offspringPlan)
    _ <- speciate[F, Double, Double](newPop)
    _ <- reassignRepresentatives[F, Double, Double]
    
    // housekeeping
    state <- getState
    _ <- setState({
      val ageIncremented = state.species.map[Species[Double]](s => s.copy(age = s.age + 1))
      state.copy(generation = state.generation + 1, species = ageIncremented, fitness = Map.empty, adjustedFitness = Map.empty)
    })
  } yield runs.last

  def evolve[F[_]: Monad: Random: Parallel: HasGameEvolutionEnv: HasGameEvolutionState: HasGenePool: StatsCallback](using RandomRange[F, Double], GenConcurrent[F, Throwable]): F[Seq[GameState]] = for {
    env <- getEnv
    genomes <- genome(valueOf[Inputs], valueOf[Outputs]).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => (id -> genome).pure)
    }.sequence
    _ <- speciate(initialPop.toMap)
    states <- List.fill(env.generations)(()).traverse { _ => step[F] }
  } yield states.last

  def run(env: GameEvolutionEnv)(using StatsCallback[IO], RandomRange[IO, Double], Random[IO]) = {
    
    def refToStateful[B](ref: Ref[IO, B]) = new Stateful[IO, B] {
      override def monad: Monad[IO] = Monad[IO]
      override def get: IO[B] = ref.get
      override def set(s: B): IO[Unit] = ref.set(s)
      override def modify(f: B => B): IO[Unit] = ref.update(f)
    }

    for {
      gameEvolutionStateRef <- Ref.of[IO, GameEvolutionState](GameEvolutionState(EvolutionState[Double, Double]()))
      genePoolStateRef <- Ref.of[IO, GenePool]{GenePool(0, Map.empty)}
      _ <- {
        given HasGameEvolutionEnv[IO] = Ask.const[IO, GameEvolutionEnv](env)
        given HasGameEvolutionState[IO] = refToStateful[GameEvolutionState](gameEvolutionStateRef)
        given HasGenePool[IO] = refToStateful[GenePool](genePoolStateRef)

        evolve[IO]
      }
      //result <- step.run(env).run(evolutionState).run(genePoolState)
    } yield ()
  } 
}
