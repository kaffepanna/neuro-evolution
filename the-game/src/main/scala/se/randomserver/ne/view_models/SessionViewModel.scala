package se.randomserver.ne.view_models

import scalafx.collections.ObservableHashMap
import se.randomserver.ne.the_game.Game.GameState
import scalafx.beans.property.LongProperty
import scalafx.beans.property.IntegerProperty
import scalafx.beans.value.ObservableValue
import scalafx.Includes.{*, given}
import scalafx.beans.property.ObjectProperty
import cats.effect.IO
import cats.effect.std.Queue
import scalafx.application.Platform
import se.randomserver.ne.GameEvolution
import scalafx.collections.ObservableMap.Change
import scalafx.collections.ObservableMap
import se.randomserver.ne.evolution.Evolution.SpeciesId
import cats.effect.std.Dispatcher
import cats.effect.kernel.Resource
import cats.effect.kernel.Fiber
import cats.effect.FiberIO
import cats.effect.std.Random
import se.randomserver.ne.BackgroundRuntime
import se.randomserver.ne.StatsCallback
import se.randomserver.ne.Stats
import se.randomserver.ne.GameEvolution.GameEvolutionEnv
import se.randomserver.ne.evolution.Evolution.EvolutionEnv
import se.randomserver.ne.genome.SpeciationConfig
import se.randomserver.ne.genome.RandomRangeConfig
import se.randomserver.ne.genome.RandomRange
import scalafx.beans.property.BooleanProperty

class SessionViewModel(runtime: BackgroundRuntime) {
  val generations = new ObservableHashMap[Long, Vector[GameState]]()
  val speciesFitness = new ObservableHashMap[Long, Map[SpeciesId, Double]]
  val currentGenerationId = LongProperty(-1)
  val currentGameStateIndex = IntegerProperty(-1)
  val ranges = ObjectProperty[RandomRangeConfig](RandomRangeConfig(
    (-2, 2), (-0.2, 0.2), (-4, 4)
  ))

  //def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))
  def transferFn(x: Double) =
    val expNeg = Math.exp(-x)
    val expPos = Math.exp(x)
    (expPos - expNeg) / (expPos + expNeg)
                              
                              
  val gameEvolutionEnv = ObjectProperty[GameEvolutionEnv](
    GameEvolutionEnv(
          teams = 1,
          gameIterations = 200,
          gamesPerGeneration =10,
          rows = 40,
          cols = 40,
          visionRadius = 2,
          evolutionEnv = EvolutionEnv[Double, Double](
            data = List.empty,
            transfer = transferFn,
            fitnessFn = (_, _) => 0,
            popsize = 40,
            generations = 1000,
            defaultBias = 1.0,
            weightChance = 0.20,
            resetChance = 0.00,
            connectionChance = 0.10,
            nodeChance = 0.10,
            eliteFraction = 0.10,
            minScore = None,
            recurrentSteps = 1,
            speciationConfig = SpeciationConfig(
              30.0, 25.0, 0.2, 1.0
            )
        )
      )
  )

  val currentGeneration = ObjectProperty[Option[Vector[GameState]]](None)

  currentGeneration <== currentGenerationId.map { id =>
      val gen = generations.get(id)
      Option.when(gen != null)(gen)
  }

  val currentGameState = ObjectProperty[Option[GameState]](None)

  currentGameState <== currentGameStateIndex.map { ix =>
    currentGeneration.value match
      case Some(gen) if ix.intValue < gen.size && ix.intValue() > 0 =>
        Some(gen(ix.intValue()))
      case _ => None
  }

  generations.onChange { (_, change) =>
    if (currentGenerationId.intValue < 0)
      change match
        case ObservableMap.Add(key, _) => 
          currentGenerationId.value = key
        case ObservableMap.Remove(_ ,_ ) =>
          ()
      
  }

  val running = BooleanProperty(false)
  val runId = LongProperty(0)

  given StatsCallback[IO] = new StatsCallback[IO] {
    override def pushGeneration(stats: Stats): IO[Unit] = IO {
      Platform.runLater {
        generations.addOne(stats.generationId, stats.game)
        speciesFitness.addOne(stats.generationId, stats.speciesFitness)
      }
    }
  }

  val traceIO: IO[Unit] = for {
    rnd <- Random.scalaUtilRandom[IO]
    given Random[IO] = rnd
    given RandomRange[IO, Double] = RandomRange(ranges()) 
    _ <- GameEvolution.run(gameEvolutionEnv())
  } yield ()

  def start() = {
    if (!running())
      runId.value += 1
      generations.clear()
      speciesFitness.clear()
      currentGameStateIndex.setValue(-1)
      running() = true
      runtime.start(traceIO)
  }

  def stop() = {
    running() = false
    runtime.stop()
  }
}
