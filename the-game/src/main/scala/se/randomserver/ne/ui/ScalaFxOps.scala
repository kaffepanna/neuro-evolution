package se.randomserver.ne.ui

import scalafx.collections.ObservableBuffer
import se.randomserver.ne.the_game.Game
import scalafx.scene.paint.Color

object ScalaFxOps {
  extension [A](source: ObservableBuffer[A]) {
    def mapObservable[B](f: A => B): ObservableBuffer[B] = {
      val target = new ObservableBuffer[B]()
        target ++= source.map(f)

        source.onChange { (_, _) =>
          target.setAll(source.map(f))
        }

        target
      }
  }

  extension (id: Game.TeamId) {
    def color: Color = {
      val hue = (id * 137.508) % 360
      Color.hsb(hue, 0.8, 0.85)
    }
  }
}
