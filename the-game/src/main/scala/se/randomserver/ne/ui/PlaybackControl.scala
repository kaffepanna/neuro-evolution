package se.randomserver.ne.ui
import se.randomserver.ne.view_models.PlaybackViewModel
import scalafx.scene.layout.HBox
import scalafx.scene.control.Button
import scalafx.beans.property.StringProperty
import scalafx.scene.control.Slider
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Priority
import scalafx.geometry.Pos
import scalafx.geometry.Insets

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

  val sliders =new VBox {
    children += playbackSlider
    children += fpsSlider
    fillWidth = true
    maxWidth = Double.MaxValue
    padding = Insets(4, 4, 4, 4)
  } 

  playPauseButton.text <== playbackViewModel.playing.map { playing =>
     if(!playing) "▶"
     else        "⏸"
  }

  playPauseButton.onAction = { _ =>
    if(playbackViewModel.playing.value)
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
