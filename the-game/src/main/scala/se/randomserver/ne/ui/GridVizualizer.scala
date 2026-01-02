package se.randomserver.ne.ui


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
import scalafx.beans.property.BufferProperty
import scalafx.Includes._


import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.effect.std.Queue
import scalafx.application.Platform
import scalafx.collections.ObservableBuffer
import scalafx.beans.property.IntegerProperty
import scalafx.scene.control.TableView
import scalafx.scene.control.TableColumn
import scalafx.beans.property.StringProperty
import scalafx.beans.binding.Bindings
import scalafx.scene.control.ComboBox
import scalafx.scene.layout.HBox
import javafx.scene.control.ListCell
import scalafx.collections.ObservableMap
import scalafx.beans.property.LongProperty
import scalafx.collections.ObservableHashMap
import se.randomserver.ne.GameEvolution
import se.randomserver.ne.ui.GridPane
import se.randomserver.ne.view_models.SessionViewModel
import se.randomserver.ne.view_models.GridViewModel
import se.randomserver.ne.view_models.PlaybackViewModel
import se.randomserver.ne.view_models.ScoreboardViewModel
import scalafx.geometry.Pos
import se.randomserver.ne.view_models.GenerationsViewModel

object GridVisualizer extends JFXApp3 {

  override def start(): Unit = {

    val sessionViewModel = new SessionViewModel()
    val gridViewModel = new GridViewModel(sessionViewModel)
    val playbackViewModel = new PlaybackViewModel(sessionViewModel)
    val scoreboardViewModel = new ScoreboardViewModel(sessionViewModel)
    val generationsViewModel = new GenerationsViewModel(sessionViewModel)

    val playbackConrol = new PlaybackControl(playbackViewModel)

    val bottomPane = new HBox {
      children += GenerationCombo(generationsViewModel)
      children += playbackConrol
      HBox.setHgrow(playbackConrol, Priority.Always)
      HBox.setMargin(playbackConrol, Insets(5, 5, 5, 5))
      alignment = Pos.Center
    }

    val borderPane = new BorderPane {
      center = GridPane(gridViewModel)
      right = Scoreboard(scoreboardViewModel)
      bottom = bottomPane
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
    }

    val container = new StackPane {
      children += borderPane
    }

    StackPane.setAlignment(borderPane, Pos.Center)

    sessionViewModel.start()

    stage = new JFXApp3.PrimaryStage {
      title = "NEAT Game"
      scene = new Scene {
        root = container
      }
    }
  }
}
