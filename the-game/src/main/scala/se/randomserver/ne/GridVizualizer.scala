package se.randomserver.ne

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
import se.randomserver.ne.ui.GameReplay
import se.randomserver.ne.view_models.SessionViewModel
import se.randomserver.ne.view_models.GameReplayViewModel
import se.randomserver.ne.view_models.PlaybackViewModel
import se.randomserver.ne.view_models.ScoreboardViewModel
import scalafx.geometry.Pos
import se.randomserver.ne.view_models.GenerationsViewModel
import se.randomserver.ne.view_models.ChartViewModel
import cats.effect.std.Dispatcher
import se.randomserver.ne.ui.ChartPane
import se.randomserver.ne.ui.PlaybackControl
import se.randomserver.ne.ui.GenerationCombo
import se.randomserver.ne.ui.Scoreboard
import scalafx.stage.WindowEvent
import scalafx.scene.control.ToolBar
import scalafx.scene.control.Button
import org.kordamp.ikonli.javafx.FontIcon
import org.kordamp.ikonli.fontawesome5.FontAwesomeSolid
import se.randomserver.ne.the_game.Game

class GridVisualizer(dispatcher: Dispatcher[IO], runtime: BackgroundRuntime)
    extends JFXApp3 {

  def shutdown(): Unit =
    dispatcher.unsafeRunAndForget {
      runtime.shutdown()
    }

  override def start(): Unit = {

    val sessionViewModel = new SessionViewModel(runtime)
    val gameViewModel = new GameReplayViewModel(sessionViewModel)
    val playbackViewModel = new PlaybackViewModel(sessionViewModel)
    val scoreboardViewModel = new ScoreboardViewModel(sessionViewModel)
    val chartViewModel = new ChartViewModel(session = sessionViewModel)

    val chart = new ChartPane(chartViewModel)

    val toolbar = new ToolBar {
      val startButton = new Button {
        graphic = new FontIcon(FontAwesomeSolid.FILE)
        onAction = { _ =>
          sessionViewModel.start()
        }
        disable <== sessionViewModel.running
      }
      val stopButton = new Button {
        graphic = new FontIcon(FontAwesomeSolid.STOP)
        onAction = { _ =>
          sessionViewModel.stop()
        }
        disable <== sessionViewModel.running.not()
      }

      content = List(
        startButton,
        stopButton
      )
    }

    val borderPane = new BorderPane {
      top = new VBox {
        children = List(toolbar, chart)
      }
      center = new GameReplay(gameViewModel)
      right = Scoreboard(scoreboardViewModel)
      bottom = PlaybackControl(playbackViewModel)
      maxHeight = Double.MaxValue
      maxWidth = Double.MaxValue
    }

    val container = new StackPane {
      children += borderPane
    }

    StackPane.setAlignment(borderPane, Pos.Center)

    // sessionViewModel.start()

    stage = new JFXApp3.PrimaryStage {
      title = "NEAT Game"
      scene = new Scene {
        root = container
      }
      onCloseRequest = (e: WindowEvent) => {
        // IMPORTANT: do NOT consume the event
        shutdown()
      }
    }
  }
}
