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

object GameEvolution {
  type Inputs = 49
  type Outputs = 4
  val ROWS = 60
  val COLS = 60

  type Evo[F[_], A] = EvolutionT[F, Double, Inputs, Outputs, Double, A]

  def integrate(
    n: Int,
    agents: Vector[((Long, EvaluatedGenome[Double, Inputs, Outputs, Double]), Long)],
    acc: Seq[GameState],
    activationStates: Map[Long, ActivationState[Double]]
  ): Seq[GameState] = n match {
    case 0 => acc
    case nn => {
      val state = acc.last
      val intents = agents.map { case ((team, member), id) =>
        val inputs = Game.vision(state, id, 3).flatten.map {
          case Cell.Empty => 0.0
          case Cell.Individual(_, `team`) => 1.0
          case Cell.Individual(_, _) => -1.0
          case Cell.Obstacle => -0.5
          case Cell.Food => 0.5
        }.zipWithIndex.map(_.swap).toMap
        val activationState = activationStates(id)
        val nextActivationState = Runner.stepNetwork(member.compiled.get, inputs, 1.0, activationState)
        val intent = member.compiled.get.outputs.toVector.sorted.map(nextActivationState.apply).zip(Game.Action.values).maxBy(_._1)
        id.toLong -> (intent._2, nextActivationState)
      }.toMap
      integrate(nn - 1, agents, acc :+ Game.step(state, intents.map((k, v) => k -> v._1)), intents.map((k, v) => k -> v._2))
    }
  }

  def gameStep[F[_]: Monad](count: Int = 1)(using RR: RandomRange[F, Double], R: Random[F]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    state <- getState[F, Double, Inputs, Outputs, Double]

    members = state.species.flatMap { species =>
      species.members.map {m => 
          val compiledMember = m.compiled match {
            case Some(_) => m 
            case _ =>
              val compiled = Compiler.compileGenome(m.genome, env.transfer)
              m.copy(compiled = Some(compiled))
          }
          (species.id, compiledMember)
      }
    }.zipWithLongIndex 
    initialIndividuals <- members.traverse { case ((teamId, member), id) =>
      for {
        r <- liftF(R.betweenInt(0, ROWS))
        c <- liftF(R.betweenInt(0, COLS))
        pos = (r, c)
        agent = Game.IndividualState(id, teamId, pos)
      } yield agent
    } 

    initialGameState = GameState.random(ROWS, COLS, initialIndividuals.toSet)
    initailActivationStates = Map.empty[Long, ActivationState[Double]].withDefault { i =>
      val ((_, member), _) = members(i.toInt)
      val maxNode = member.compiled.get.blocks.flatMap(_.nodes.map(_.id)).max
      ActivationState.zero[Double](maxNode + 1)
    }
    states = integrate(env.generations, members, Seq(initialGameState), initailActivationStates)
    updatedSpecies = members.groupBy(_._1._1).map { 
      case (speciesId, m) =>
        val species = state.species.find(_.id == speciesId).get
        val updatedMembers = m.map {
          case ((_, member), id) =>
            val oldScore = member.rawFitness.getOrElse(0d)
            val newScore = oldScore + (states.last.individuals(id).score - oldScore) / count.toDouble
            member.copy(rawFitness = Some(newScore))
        }
        species.copy(members = updatedMembers)
    }

    _ <- modifyState[F, Double, Inputs, Outputs, Double] { s =>
      state.copy(species = updatedSpecies.toVector)
    }
  } yield states

  def step[F[_]: Monad](using R: Random[F], RR: RandomRange[F, Double]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    states <- Range.inclusive(1, env.recurrentSteps)
                   .toList
                   .traverse(n => gameStep(n))
    _ <- adjustFitnessSharing[F, Double, Inputs, Outputs, Double]
    _ <- debug[F, Double, Inputs, Outputs, Double]
    _ <- cullSpecies[F, Double, Inputs, Outputs, Double]
    offspringPlan <- allocateOffspringPerSpecies[F, Double, Inputs, Outputs, Double]
    _ = { println(s"Offspring plan size ${offspringPlan}")}
    newPop <- reproduce[F, Double, Inputs, Outputs, Double](offspringPlan)
    _ <- speciate[F, Double, Inputs, Outputs, Double](newPop)
    _ <- reassignRepresentatives[F, Double, Inputs, Outputs, Double]
    _ <- EvolutionT.modifyState[F, Double, Inputs, Outputs, Double] { state =>
      val ageIncremented = state.species.map(s => s.copy(age = s.age + 1))
      state.copy(generation = state.generation + 1, species = ageIncremented)
    }
  } yield states.last

  def evolve[F[_]: Monad: Random](using RandomRange[F, Double]): Evo[F, Seq[GameState]] = for {
    env <- getEnv
    initialPop <- liftGenePool(genome[F, Double, Inputs, Outputs]).replicateA(env.popsize)
    _ <- speciate(initialPop.toVector)
    states <- List.fill(env.generations)(()).traverse { _ => step }
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
        popsize = 35,
        generations = 200,
        defaultBias = 1.0,
        weightChance = 0.35,
        resetChance = 0,
        connectionChance = 0.25,
        nodeChance = 0.12,
        eliteFraction = 0.10,
        minScore = None,
        recurrentSteps = 4,
        speciationConfig = SpeciationConfig(
          4.0, 3.0, 0.4, 0.25
        )
      )
      evolutionState = EvolutionState[Double, Inputs, Outputs, Double](
        Vector.empty, 0
      )

      genePoolState = GenePool(0, Map.empty)
      result <- evolve[IO].runEvolution(env, evolutionState, genePoolState)
      (genePool, (evolutionState, gameStates)) = result

      //result <- step.run(env).run(evolutionState).run(genePoolState)
    } yield gameStates
  } 
}
