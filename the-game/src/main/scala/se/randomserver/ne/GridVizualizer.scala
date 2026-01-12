package se.randomserver.ne

import cats.effect.IO
import cats.effect.std.Dispatcher
import org.kordamp.ikonli.fontawesome5.FontAwesomeSolid
import org.kordamp.ikonli.javafx.FontIcon
import scalafx.Includes.*
import scalafx.application.JFXApp3
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.control.ToolBar
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.VBox
import scalafx.stage.WindowEvent
import se.randomserver.ne.ui.ChartPane
import se.randomserver.ne.ui.GameReplay
import se.randomserver.ne.ui.PlaybackControl
import se.randomserver.ne.ui.Scoreboard
import se.randomserver.ne.view_models.ChartViewModel
import se.randomserver.ne.view_models.GameReplayViewModel
import se.randomserver.ne.view_models.PlaybackViewModel
import se.randomserver.ne.view_models.ScoreboardViewModel
import se.randomserver.ne.view_models.SessionViewModel

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
      onCloseRequest = (_: WindowEvent) => {
        // IMPORTANT: do NOT consume the event
        shutdown()
      }
    }
  }
}
