package se.randomserver.ne.ui
import org.kordamp.ikonli.fontawesome5.FontAwesomeSolid
import org.kordamp.ikonli.javafx.FontIcon
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.control.Button
import scalafx.scene.control.Slider
import scalafx.scene.layout.HBox
import scalafx.scene.layout.Priority
import scalafx.scene.layout.VBox
import se.randomserver.ne.view_models.PlaybackViewModel

class PlaybackControl(playbackViewModel: PlaybackViewModel) extends HBox {
  val playPauseButton = new Button {
    padding = Insets(4, 4, 4, 4)
  }

  val playbackSlider = new Slider() {
    showTickLabels = true
    showTickMarks = true
    max <== playbackViewModel.indexMax
    min = 0
    blockIncrement = 1
    value <==> playbackViewModel.index
  }

  val fpsSlider = new Slider(0, 60, 0) {
    showTickLabels = true
    showTickMarks = true
    blockIncrement = 1
    value <==> playbackViewModel.speedFps
  }

  val sliders = new VBox {
    children += playbackSlider
    children += fpsSlider
    fillWidth = true
    maxWidth = Double.MaxValue
    padding = Insets(4, 4, 4, 4)
  }

  val playIcon = new FontIcon(FontAwesomeSolid.PLAY)
  val pauseIcon = new FontIcon(FontAwesomeSolid.PAUSE)

  playPauseButton.graphic <== playbackViewModel.playing.map { playing =>
    if (!playing) playIcon
    else pauseIcon
  }

  playPauseButton.onAction = { _ =>
    if (playbackViewModel.playing.value)
      playbackViewModel.pause()
    else
      playbackViewModel.play()
  }

  HBox.setHgrow(sliders, Priority.Always)
  alignment = Pos.CenterLeft

  children ++= Seq(
    playPauseButton,
    sliders
  )
}
