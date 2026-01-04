package se.randomserver.ne.ui

import se.randomserver.ne.view_models.ChartViewModel
import scalafx.scene.layout.Pane
import scalafx.scene.chart.ValueAxis
import scalafx.scene.chart.LineChart
import scalafx.scene.chart.NumberAxis
import scalafx.scene.layout.StackPane

class ChartPane(viewModel: ChartViewModel) extends StackPane {
  val genAxis = NumberAxis("Generations")
  val fitAxis = NumberAxis("Fitness")
  val chart = new LineChart(genAxis, fitAxis, viewModel.series) {
    title = "Evolution"
    maxWidth = Double.MaxValue
  }

  genAxis.setAutoRanging(true)
  


  maxWidth = Double.MaxValue
  children += chart
}
