package se.randomserver.ne.the_game


import scalafx.application.JFXApp3
import scalafx.scene.Scene
import scalafx.scene.layout.BorderPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.scene.control.{Slider, Tooltip}
import scalafx.animation.AnimationTimer
import se.randomserver.ne.the_game.Game.GameState
import se.randomserver.ne.the_game.Game.Cell
import scala.util.Random
import se.randomserver.ne.the_game.Game.IndividualState
import scalafx.scene.layout.ColumnConstraints
import scalafx.scene.layout.Priority
import scalafx.scene.layout.RowConstraints
import scalafx.geometry.Insets
import scalafx.scene.layout.StackPane
import scalafx.scene.canvas.Canvas
import scalafx.scene.layout.VBox
import scalafx.scene.control.Label
import scalafx.scene.layout.Pane
import scalafx.beans.property.ObjectProperty

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object GridVisualizer extends JFXApp3 {

  val N = GameEvolution.COLS
  val M = GameEvolution.ROWS



  val traceIO: IO[Seq[GameState]] = GameEvolution.run


  val trace = traceIO.unsafeRunSync()


  val gameStateProperty = ObjectProperty[GameState](trace.head)

  var frame = 0


  override def start(): Unit = {


    val canvas = new GridPane(gameStateProperty)
   
    val fpsSlider = new Slider(0, 60, 10)
    fpsSlider.showTickLabels = true
    fpsSlider.showTickMarks = true
    fpsSlider.blockIncrement = 1
    fpsSlider.value = 0

    val scoreboard = new VBox {
      spacing = 5
    }

    val rootPane = new BorderPane {
      center = canvas
      right = scoreboard
      bottom = fpsSlider
    }

    gameStateProperty.onChange {
      updateScoreboard(scoreboard)
    }
    
    stage = new JFXApp3.PrimaryStage {
      title = "Canvas Grid Visualizer"
      scene = new Scene(650, 700) {
        root = rootPane
      }
    }

    // AnimationTimer with adjustable FPS
    var lastUpdate: Long = 0L
    val timer = AnimationTimer { now =>
      val targetFPS = fpsSlider.value.value
      val targetDelayNs = (1e9 / targetFPS).toLong

      if (now - lastUpdate >= targetDelayNs) {
        lastUpdate = now
        frame += 1
        if (frame < trace.size)
          gameStateProperty.value = trace(frame)
      }
    }
  
    timer.start()
  }

  // Update scores each frame:
  def updateScoreboard(scoreboard: VBox): Unit = {
    scoreboard.children.clear()
    gameStateProperty.value.individuals.values.toSeq.sortBy(-_.score).foreach { ind =>
      scoreboard.children.add(new Label(s"ID: ${ind.id}, Team: ${ind.team}, Score: ${String.format("%.2f", ind.score)}"))
    }
  }
}
