package se.randomserver.ne
import cats.* 
import cats.syntax.all.*
import cats.syntax.functor.*
import se.randomserver.ne.evolution.Evolution.{*, given}
import cats.effect.std.Random
import cats.effect.Ref
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

  final case class GameEvolutionEnv[F[_]](
    teams: Int,
    gameIterations: Int,
    gamesPerGeneration: Int,
    rows: Int,
    cols: Int,
    visionRadius: Int,
    gameStateReport: Queue[F, Vector[GameState]],
    fitnessReport: Queue[F, Map[SpeciesId, Double]],
    evolutionEnv: EvolutionEnv[Double, Double]
  ) {
    export evolutionEnv.*
    lazy val inputs: Int = (2* visionRadius  + 1)**2
    lazy val outputs: Int = Game.Action.values.size
  }

  final case class GameEvolutionState(
    evolutionState: EvolutionState[Double, Double]
  )

  type HasGameEvolutionEnv[F[_]] = Ask[F, GameEvolutionEnv[F]]
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

  def getGameEnv[F[_]: HasGameEvolutionEnv]: F[GameEvolutionEnv[F]] = Ask[F, GameEvolutionEnv[F]].ask

  def integrate[F[_]: Monad: Parallel](
    n: Int,
    agents: Map[GenomeId, (SpeciesId, CompiledNetwork[Double])],
    acc: Vector[GameState],
    activationStates: Map[GenomeId, ActivationState[Double]]
  ): F[Vector[GameState]] = n match {
    case 0 => Monad[F].pure(acc)
    case nn => {
      val state = acc.last
      val intentsF = agents.map { case (id, (team, member)) =>
        val inputs = Game.vision(state, id, valueOf[VisionRadius]).flatten.map {
          case Cell.Empty => 0.0
          case Cell.Individual(_, `team`) => 0.5
          case Cell.Individual(_, _) => -1.0
          case Cell.Obstacle => -0.5
          case Cell.Food => 1.0
        }.zip(member.inputs.toVector.sorted).map(_.swap).toMap
        val activationState = activationStates(id)
        val nextActivationState = Runner.stepNetwork(member, inputs, 1.0, activationState)
        val intent = member.outputs.toVector.sorted.map(nextActivationState.apply).zip(Game.Action.values).maxBy(_._1)
        Monad[F].pure(id -> (intent._2, nextActivationState))
      }.toList.sequence.map(_.toMap)
      intentsF >>= { intents => 
        val nextGameState = Game.step(state, intents.map((k, v) => k -> v._1))
        val stuck = (acc :+ nextGameState).reverse.take(5).map(a => a.individuals.values.map(_.score).sum).toSet.size == 1
        if (stuck) (acc :+ nextGameState).pure
        else integrate(nn - 1, agents, acc :+ nextGameState, intents.map((k, v) => k -> v._2))
      }
    }
  }

  def gameStep[F[_]: Monad: Applicative: Parallel: HasGameEvolutionEnv](count: Int = 1, members: Vector[(Int, CompiledNetwork[Double])])(using RR: RandomRange[F, Double], R: Random[F]): F[(Vector[GameState], Map[GenomeId, Double])] = for {
    gameEnv <- getGameEnv
    shuffled <- Random[F].shuffleVector(members.toVector)
    teams = Utils.splitEvenly(shuffled, gameEnv.teams).zipWithIndex.flatMap {
      case (a, team) => a.map {
        case (id, m) => (id, (team, m))
      }
    }.toMap
    
    initialIndividuals <- Monad[F].pure(teams.toVector.map { case id -> (teamId, member) => id -> teamId })

    initialGameState = GameState.random(ROWS, COLS, initialIndividuals.toSet)
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

  def evaluateFitness[F[_]: Monad: Parallel: Random: HasGameEvolutionEnv: HasGameEvolutionState](using RandomRange[F, Double]): F[Vector[Vector[GameState]]] = for {
    env <- getGameEnv
    state <- getState
    
    members = state.population.map { (id, genome) =>
          val compiled = Compiler.compileGenome(genome, env.transfer)
          id -> compiled
    }.toVector
    
     
    runs <- (1 to env.gamesPerGeneration).inclusive
        .toVector
        .map(n => gameStep(n, members)).parSequence

    updatedFitness = runs.map(_._2).foldLeft(Map.empty[Int, (Double, Int)]) { (acc, map) =>
      map.foldLeft(acc) { case (innerAcc, (k, v)) =>
        val (sum, count) = innerAcc.getOrElse(k, (0.0, 0))
        innerAcc.updated(k, (sum + v, count + 1))
      }  
    }.map { case (k, (v, n)) => k -> v / n }
    
    _ <- setState(state.copy(fitness = updatedFitness))
  } yield runs.map(_._1)

  def step[F[_]: Monad: Parallel: HasGameEvolutionEnv: HasGameEvolutionState: HasGenePool](using R: Random[F], RR: RandomRange[F, Double]): F[Vector[GameState]] = for {
    env <- getGameEnv
    
    runs <- evaluateFitness
    _ <- env.gameStateReport.offer(runs.last)
    _ <- adjustFitnessSharing[F, Double, Double]
    _ <- getState.flatMap { state => 
      val speciesFitnesses = state.species.map { species =>
        val fitnessSum = species.members.map(state.fitness).foldLeft(0d)(_ + _)
        species.id -> fitnessSum / species.members.size
      }
      env.fitnessReport.offer(speciesFitnesses.toMap)
    }
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

  def evolve[F[_]: Monad: Random: Parallel: HasGameEvolutionEnv: HasGameEvolutionState: HasGenePool](using RandomRange[F, Double]): F[Seq[GameState]] = for {
    env <- getEnv
    genomes <- genome(valueOf[Inputs], valueOf[Outputs]).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => (id -> genome).pure)
    }.sequence
    _ <- speciate(initialPop.toMap)
    states <- List.fill(env.generations)(()).traverse { _ => step[F] }
  } yield states.last

  def run(queue: Queue[IO, Vector[GameState]], speciesFitnessQueue: Queue[IO, Map[SpeciesId, Double]]): IO[Unit] = {
    def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))

    given [F[_]: Monad, B]: Conversion[Ref[F, B], Stateful[F, B]] = (ref: Ref[F, B]) => new Stateful[F, B] {
      override def monad: Monad[F] = Monad[F]
      override def get: F[B] = ref.get
      override def set(s: B): F[Unit] = ref.set(s)
      override def modify(f: B => B): F[Unit] = ref.update(f)
    }

    for {
      rnd <- Random.scalaUtilRandom[IO]
      given Random[IO] = rnd
      given RandomRange[IO, Double] = RandomRange(
        (-1.0, 1.0),
        (-0.05, 0.05),
        (-1.0, 1.0)
      )
      env = GameEvolutionEnv[IO](
          teams = 1,
          gameIterations = 200,
          gamesPerGeneration = 20,
          rows = 30,
          cols = 30,
          visionRadius = 3,
          gameStateReport = queue,
          fitnessReport = speciesFitnessQueue,
          evolutionEnv = EvolutionEnv[Double, Double](
            data = List.empty,
            transfer = transferFn,
            fitnessFn = (_, _) => 0,
            popsize = 30,
            generations = 1000,
            defaultBias = 1.0,
            weightChance = 0.15,
            resetChance = 0.00,
            connectionChance = 0.25,
            nodeChance = 0.10,
            eliteFraction = 0.10,
            minScore = None,
            recurrentSteps = 1,
            speciationConfig = SpeciationConfig(
              20.0, 15.0, 0.1, 0.5
            )
        )
      )

      gameEvolutionStateRef <- Ref.of[IO, GameEvolutionState](GameEvolutionState(EvolutionState[Double, Double]()))
      genePoolStateRef <- Ref.of[IO, GenePool](GenePool(0, Map.empty))

      given HasGameEvolutionEnv[IO] = Ask.const[IO, GameEvolutionEnv[IO]](env)
      given HasGameEvolutionState[IO] = given_Conversion_Ref_Stateful[IO, GameEvolutionState](gameEvolutionStateRef)//gameEvolutionStateRef
      given HasGenePool[IO] = given_Conversion_Ref_Stateful[IO, GenePool](genePoolStateRef)

      _ <- evolve[IO]
      //result <- step.run(env).run(evolutionState).run(genePoolState)
    } yield ()
  } 
}
