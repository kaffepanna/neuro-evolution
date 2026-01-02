package se.randomserver.ne.view_models

import scalafx.beans.property.BooleanProperty
import scalafx.beans.property.IntegerProperty
import scalafx.animation.AnimationTimer
import scalafx.beans.binding.Bindings

class PlaybackViewModel(session: SessionViewModel) {
  val playing = BooleanProperty(false)
  val speedFps = IntegerProperty(5)
  val indexMax = Bindings.createIntegerBinding( () =>
    { 
      session.currentGeneration.value match
        case Some(gen) => gen.size - 1
        case _ => 0
    }, session.currentGeneration
  )
  val index = IntegerProperty(0)
  index <==> session.currentGameStateIndex

  private var lastUpdate = 0L

  session.currentGeneration.onChange { (_, _, gen) =>
    index.value = 0
  }
  
  val timer = AnimationTimer { now =>
    val targetFPS = speedFps.value
    val targetDelayNs = if (targetFPS > 0) (1e9 / targetFPS).toLong else Long.MaxValue

    if (now - lastUpdate >= targetDelayNs) {
      lastUpdate = now
      stepForward()
    }
  }

  def stepForward(): Unit = {
    val intIndex = index.intValue()
    val intMax = indexMax.intValue()
    if (intIndex + 1 < intMax)
      index.value = intIndex + 1
    else
      pause()
  }

  def play(): Unit = {
    playing.value = true
    timer.start()
  }

  def pause(): Unit = {
    playing.value = false
    timer.stop()
  }
}
