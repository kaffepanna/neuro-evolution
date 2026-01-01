package se.randomserver.ne.the_game
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

object GameEvolution {
  type Inputs = 49
  type Outputs = 4
  val ROWS = 30
  val COLS = 30

  type Evo[F[_], A] = EvolutionT[F, Double, Inputs, Outputs, Double, A]

  def integrate[F[_]: Monad: Parallel](
    n: Int,
    agents: Map[GenomeId, (SpeciesId, CompiledNetwork[Double])],
    acc: Array[GameState],
    activationStates: Map[GenomeId, ActivationState[Double]]
  ): F[Array[GameState]] = n match {
    case 0 => Monad[F].pure(acc)
    case nn => {
      val state = acc.last
      val intentsF = agents.map { case (id, (team, member)) =>
        val inputs = Game.vision(state, id, 3).flatten.map {
          case Cell.Empty => 0.0
          case Cell.Individual(_, `team`) => 1.0
          case Cell.Individual(_, _) => -1.0
          case Cell.Obstacle => -0.5
          case Cell.Food => 0.5
        }.zip(member.inputs.toVector.sorted).map(_.swap).toMap
        val activationState = activationStates(id)
        val nextActivationState = Runner.stepNetwork(member, inputs, 1.0, activationState)
        val intent = member.outputs.toVector.sorted.map(nextActivationState.apply).zip(Game.Action.values).maxBy(_._1)
        Monad[F].pure(id -> (intent._2, nextActivationState))
      }.toList.parSequence.map(_.toMap)
      intentsF >>= { intents => integrate(nn - 1, agents, acc :+ Game.step(state, intents.map((k, v) => k -> v._1)), intents.map((k, v) => k -> v._2)) }
    }
  }

  def gameStep[F[_]: Monad: Applicative: Parallel](count: Int = 1, env: EvolutionEnv[Double, Double], members:  Map[Int, (Int, CompiledNetwork[Double])])(using RR: RandomRange[F, Double], R: Random[F]): F[(Seq[GameState], Map[GenomeId, Double])] = for {
    initialIndividuals <- Monad[F].pure(members.toVector.map { case id -> (teamId, member) => id -> teamId })

    initialGameState = GameState.random(ROWS, COLS, initialIndividuals.toSet)
    initailActivationStates = Map.empty[GenomeId, ActivationState[Double]].withDefault { i =>
      val (_, compiled) = members(i)
      val maxNode = compiled.blocks.flatMap(_.nodes.map(_.id)).max
      ActivationState.zero[Double](maxNode + 1)
    }
    states <- integrate(env.generations, members, Array(initialGameState), initailActivationStates)
    
    updatedFitness = members.map { case (id, (_, _)) =>
      id -> states.last.individuals(id).score
    }.toMap
  } yield (states, updatedFitness)

  def step[F[_]: Monad: Parallel](using R: Random[F], RR: RandomRange[F, Double]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    state <- getState[F, Double, Inputs, Outputs, Double]
    
    members = state.population.map { (id, genome) =>
          val compiled = Compiler.compileGenome(genome, env.transfer)
          id -> compiled
    }.toVector
    
    shuffled <- liftF(R.shuffleVector(members))
    teams = Utils.splitEvenly(shuffled, 4).zipWithIndex.flatMap {
      case (a, team) => a.map {
        case (id, m) => (id, (team, m))
      }
    }.toMap
    
    runs <- liftF { 
      (1 to env.recurrentSteps).inclusive
        .toList
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
  } yield runs.last._1

  def evolve[F[_]: Monad: Random: Parallel](using RandomRange[F, Double]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    genomes <- liftGenePool[F, Double, Inputs, Outputs, Double, Genome[Double, Inputs, Outputs]](genome()).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => EvolutionT.pure[F, Double, Inputs, Outputs, Double, (GenomeId, Genome[Double, Inputs, Outputs])](id -> genome))
    }.sequence
    _ <- speciate(initialPop.toMap)
    states <- List.fill(env.generations)(()).traverse { _ => step[F] }
  } yield states.last

  def run: IO[Seq[GameState]] = {
    def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))

    for {
      rnd <- Random.scalaUtilRandom[IO]
      given Random[IO] = rnd
      given RandomRange[IO, Double] = RandomRange(
        (-1.0, 1.0),
        (-0.1, 0.1),
        (-3.0, 3.0)
      )
      env = EvolutionEnv[Double, Double](
        data = List.empty,
        transfer = transferFn,
        fitnessFn = (_, _) => 0,
        popsize = 20,
        generations = 500,
        defaultBias = 1.0,
        weightChance = 0.50,
        resetChance = 0,
        connectionChance = 0.15,
        nodeChance = 0.10,
        eliteFraction = 0.10,
        minScore = None,
        recurrentSteps = 5,
        speciationConfig = SpeciationConfig(
          15.0, 10.0, 0.2, 0.3
        )
      )
      evolutionState = EvolutionState[Double, Inputs, Outputs, Double]()

      genePoolState = GenePool(0, Map.empty)
      result <- evolve[IO].runEvolution(env, evolutionState, genePoolState)
      (genePool, (evolutionState, gameStates)) = result

      //result <- step.run(env).run(evolutionState).run(genePoolState)
    } yield gameStates
  } 
}
