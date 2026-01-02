package se.randomserver.ne
import cats.* 
import cats.syntax.all.*
import se.randomserver.ne.evolution.Evolution.{*, given}
import cats.effect.std.Random
import se.randomserver.ne.the_game.Game.GameState
import se.randomserver.ne.genome.GenePool.{*, given}
import se.randomserver.ne.genome.GenePool
import se.randomserver.ne.evolution.Evolution.EvolutionT.{getState, liftF, getEnv, liftGenePool}
import se.randomserver.ne.evaluator.{Compiler, Runner}
import se.randomserver.ne.evolution.Evolution.EvolutionT.pure
import se.randomserver.ne.the_game.Game.Cell
import se.randomserver.ne.the_game.Game.IndividualState
import se.randomserver.ne.evolution.Evolution.EvolutionT.modifyState
import cats.effect.IO
import se.randomserver.ne.genome.SpeciationConfig
import se.randomserver.ne.genome.RandomRange
import se.randomserver.ne.evaluator.Runner.ActivationState
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork
import se.randomserver.ne.genome.Genome
import cats.effect.std.Queue
import se.randomserver.ne.the_game.Game
import se.randomserver.ne.the_game.Utils

object GameEvolution {
  import scala.compiletime.ops.int.*
  type VisionRadius = 2
  type VisionSqrt = VisionRadius * 2 + 1
  type Inputs = VisionSqrt * VisionSqrt
  type Outputs = 4
  val ROWS = 30
  val COLS = 30

  type Evo[F[_], A] = EvolutionT[F, Double, Inputs, Outputs, Double, A]

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
        //val result = Runner.evalNetwork(member, inputs, 0.0, 5)
        val nextActivationState = Runner.stepNetwork(member, inputs, 1.0, activationState)
        val intent = member.outputs.toVector.sorted.map(nextActivationState.apply).zip(Game.Action.values).maxBy(_._1)
        //val intent = result.zip(Game.Action.values).maxBy(_._1)
        Monad[F].pure(id -> (intent._2, nextActivationState))
        //Monad[F].pure(id -> (intent._2, activationState))

      }.toList.parSequence.map(_.toMap)
      intentsF >>= { intents => integrate(nn - 1, agents, acc :+ Game.step(state, intents.map((k, v) => k -> v._1)), intents.map((k, v) => k -> v._2)) }
    }
  }

  def gameStep[F[_]: Monad: Applicative: Parallel](count: Int = 1, env: EvolutionEnv[Double, Double], members:  Map[Int, (Int, CompiledNetwork[Double])])(using RR: RandomRange[F, Double], R: Random[F]): F[(Vector[GameState], Map[GenomeId, Double])] = for {
    initialIndividuals <- Monad[F].pure(members.toVector.map { case id -> (teamId, member) => id -> teamId })

    initialGameState = GameState.random(ROWS, COLS, initialIndividuals.toSet)
    initailActivationStates = Map.empty[GenomeId, ActivationState[Double]].withDefault { i =>
      val (_, compiled) = members(i)
      val maxNode = compiled.blocks.flatMap(_.nodes.map(_.id)).max
      ActivationState.zero[Double](maxNode + 1)
    }
    states <- integrate(60, members, Vector(initialGameState), initailActivationStates)
    
    updatedFitness = members.map { case (id, (_, _)) =>
      id -> states.last.individuals(id).score
    }.toMap
  } yield (states, updatedFitness)

  def evaluateFitness[F[_]: Monad: Parallel: Random](using RandomRange[F, Double]): Evo[F, Vector[Vector[GameState]]] = for {
    env <- getEnv
    state <- getState[F, Double, Inputs, Outputs, Double]
    
    members = state.population.map { (id, genome) =>
          val compiled = Compiler.compileGenome(genome, env.transfer)
          id -> compiled
    }.toVector
    
    shuffled <- liftF(Random[F].shuffleVector(members))
    teams = Utils.splitEvenly(shuffled, 1).zipWithIndex.flatMap {
      case (a, team) => a.map {
        case (id, m) => (id, (team, m))
      }
    }.toMap
    
    runs <- liftF { 
      (1 to env.recurrentSteps).inclusive
        .toVector
        .map(n => gameStep(n, env, teams)).parSequence
    }

    _ <- modifyState[F, Double, Inputs, Outputs, Double] { state => 
      val updatedFitness = runs.map(_._2).foldLeft(Map.empty[Int, (Double, Int)]) { (acc, map) =>
        map.foldLeft(acc) { case (innerAcc, (k, v)) =>
          val (sum, count) = innerAcc.getOrElse(k, (0.0, 0))
          innerAcc.updated(k, (sum + v, count + 1))
        }  
      }.map { case (k, (v, n)) => k -> v / n }
      state.copy(fitness = updatedFitness)
    }
  } yield runs.map(_._1)

  def step[F[_]: Monad: Parallel](queue: Queue[F, Vector[GameState]])(using R: Random[F], RR: RandomRange[F, Double]): Evo[F, Vector[GameState]] = for {
    env <- getEnv
    state <- getState[F, Double, Inputs, Outputs, Double]
    
    runs <- evaluateFitness
    _ <- EvolutionT.liftF(queue.offer(runs.last))
    _ <- adjustFitnessSharing[F, Double, Inputs, Outputs, Double]
    _ <- debug[F, Double, Inputs, Outputs, Double]
    _ <- cullSpecies[F, Double, Inputs, Outputs, Double]
    offspringPlan <- allocateOffspringPerSpecies[F, Double, Inputs, Outputs, Double]
    newPop <- reproduce[F, Double, Inputs, Outputs, Double](offspringPlan)
    _ <- speciate[F, Double, Inputs, Outputs, Double](newPop)
    _ <- reassignRepresentatives[F, Double, Inputs, Outputs, Double]
    _ <- EvolutionT.modifyState[F, Double, Inputs, Outputs, Double] { state =>
      val ageIncremented = state.species.map[Species[Double, Inputs, Outputs]](s => s.copy(age = s.age + 1))
      state.copy(generation = state.generation + 1, species = ageIncremented, fitness = Map.empty, adjustedFitness = Map.empty)
    }
  } yield runs.last

  def evolve[F[_]: Monad: Random: Parallel](queue: Queue[F, Vector[GameState]])(using RandomRange[F, Double]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    genomes <- liftGenePool[F, Double, Inputs, Outputs, Double, Genome[Double, Inputs, Outputs]](genome()).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => EvolutionT.pure[F, Double, Inputs, Outputs, Double, (GenomeId, Genome[Double, Inputs, Outputs])](id -> genome))
    }.sequence
    _ <- speciate(initialPop.toMap)
    states <- List.fill(env.generations)(()).traverse { _ => step[F](queue) }
  } yield states.last

  def run(queue: Queue[IO, Vector[GameState]]): IO[Unit] = {
    def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))

    for {
      rnd <- Random.scalaUtilRandom[IO]
      given Random[IO] = rnd
      given RandomRange[IO, Double] = RandomRange(
        (-1.0, 1.0),
        (-0.2, 0.2),
        (-4.0, 4.0)
      )
      env = EvolutionEnv[Double, Double](
        data = List.empty,
        transfer = transferFn,
        fitnessFn = (_, _) => 0,
        popsize = 20,
        generations = 1000,
        defaultBias = 1.0,
        weightChance = 0.10,
        resetChance = 0.01,
        connectionChance = 0.10,
        nodeChance = 0.05,
        eliteFraction = 0.10,
        minScore = None,
        recurrentSteps = 10,
        speciationConfig = SpeciationConfig(
          15.0, 10.0, 0.1, 0.35
        )
      )
      evolutionState = EvolutionState[Double, Inputs, Outputs, Double]()

      genePoolState = GenePool(0, Map.empty)
      result <- evolve[IO](queue).runEvolution(env, evolutionState, genePoolState)
      (genePool, (evolutionState, gameStates)) = result

      //result <- step.run(env).run(evolutionState).run(genePoolState)
    } yield ()
  } 
}
