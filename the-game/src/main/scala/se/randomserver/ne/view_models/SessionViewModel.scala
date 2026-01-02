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

class SessionViewModel {
  val generations = new ObservableHashMap[Long, Vector[GameState]]()
  val currentGenerationId = LongProperty(-1)
  val currentGameStateIndex = IntegerProperty(-1)

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

  val traceIO: IO[Unit] = for {
    queue <- Queue.unbounded[IO, Vector[GameState]]
    stream = fs2.Stream.fromQueueUnterminated(queue).zipWithIndex.evalTap { (state, id) =>
      IO { 
        Platform.runLater {
          generations.addOne(id -> state)
        }
      }
    }.compile.drain
    _ <- (GameEvolution.run(queue), stream).parTupled
  } yield ()

  def start() = {
    import cats.effect.unsafe.implicits.global
    traceIO.unsafeRunAndForget()
  }
}
