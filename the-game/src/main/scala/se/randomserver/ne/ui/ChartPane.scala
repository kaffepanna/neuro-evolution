package se.randomserver.ne.ui

import se.randomserver.ne.view_models.ChartViewModel
import scalafx.Includes.*
import scalafx.scene.layout.Pane
import scalafx.scene.chart.ValueAxis
import scalafx.scene.chart.LineChart
import scalafx.scene.chart.NumberAxis
import scalafx.scene.layout.StackPane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Line
import scalafx.application.Platform
import scalafx.scene.input.MouseEvent
import scalafx.scene.shape.StrokeType

class ChartPane(viewModel: ChartViewModel) extends StackPane {
  val selectionLine = new Line {
    stroke = Color.LightSalmon
    visible = false
    strokeWidth = 3
  }

  val mouseLine = new Line {
    stroke = Color.LightBlue
    strokeWidth = 2
  }

  val genAxis = NumberAxis("Generations")
  val fitAxis = NumberAxis("Fitness")
  val chart = new LineChart(genAxis, fitAxis, viewModel.series) {
    title = "Evolution"
    maxWidth = Double.MaxValue
  }
  val overlayPane = new Pane {
    children ++= List(selectionLine, mouseLine)
  }

  genAxis.setAnimated(false)

  def overlayToAxisX(axis: NumberAxis, overlayX: Double, overlayPane: Pane): Double = {
    val scenePoint = overlayPane.localToScene(overlayX, 0)
    val axisLocalX = axis.sceneToLocal(scenePoint).getX
    axis.getValueForDisplay(axisLocalX).doubleValue() // data value
  }

  def axisXToOverlay(axis: NumberAxis, xValue: Number): Double = {
    val xInAxis = axis.getDisplayPosition(xValue)

    val pointInScene =
      axis.localToScene(xInAxis, 0)

    val pointInOverlay =
      overlayPane.sceneToLocal(pointInScene)

    pointInOverlay.getX
  }

  def axisYToOverlay(axis: NumberAxis, yValue: Number): Double = {
    val yInAxis = axis.getDisplayPosition(yValue)

    val pointInScene =
      axis.localToScene(0, yInAxis)

    val pointInOverlay =
      overlayPane.sceneToLocal(pointInScene)

    pointInOverlay.getY
  }

  def overlayLine(line: Line, value: Number) =
    Platform.runLater {
        if (value != null && value.intValue >= 0) {
          val x = axisXToOverlay(genAxis, value)
          if (!x.isNaN) {
            line.startX = x
            line.endX   = x
            line.startY = axisYToOverlay(fitAxis, fitAxis.lowerBound())
            line.endY   = axisYToOverlay(fitAxis, fitAxis.upperBound())
            line.visible = true
          }
        } else {
          line.visible = false
        }
      }

  def updateCurrentGenerationFromX(mouseX: Double): Unit = {
    val genValue = overlayToAxisX(genAxis, mouseX, overlayPane)
    
    // Snap to integer if generation IDs are integers
    val snappedGen = Math.round(genValue).toInt
    viewModel.session.currentGenerationId.set(snappedGen)
  }

  genAxis.lowerBound.onChange { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  genAxis.upperBound.onChange { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  fitAxis.lowerBound.onChange { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  fitAxis.upperBound.onChange { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  chart.widthProperty.onChange  { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  chart.heightProperty.onChange { (_, _, _) => overlayLine(selectionLine, viewModel.session.currentGenerationId()) }
  viewModel.session.currentGenerationId.onChange { (_, _, gen) => overlayLine(selectionLine, gen) }

  overlayPane.onMouseClicked = (e: MouseEvent) => {
    updateCurrentGenerationFromX(e.x)
  }

  overlayPane.onMouseMoved = (e: MouseEvent) => {
    val genValue = overlayToAxisX(genAxis, e.x, overlayPane)
    val snappedGen = Math.round(genValue).toInt
    overlayLine(mouseLine, snappedGen)
  }


  maxWidth = Double.MaxValue
  children = Seq(chart, overlayPane)
}
