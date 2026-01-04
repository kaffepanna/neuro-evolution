package se.randomserver.ne.view_models

import scalafx.collections.ObservableBuffer
import scalafx.scene.chart.XYChart.Series
import scalafx.scene.chart.XYChart.Data
import javafx.scene.chart.XYChart.{Series => JFXSeries}
import scalafx.collections.ObservableMap.Change
import scalafx.collections.ObservableMap
import scalafx.Includes.*
import scalafx.collections.ObservableHashMap
import se.randomserver.ne.evolution.Evolution.SpeciesId
import scalafx.beans.binding.Bindings

class ChartViewModel(session: SessionViewModel) {
  val seriesMap: ObservableHashMap[SpeciesId, JFXSeries[Number, Number]] = ObservableHashMap[SpeciesId, JFXSeries[Number, Number]]()
    //.withDefault(speciesId => Series[Number, Number](s"Species: ${speciesId}", ObservableBuffer())) //(averageSeries, topSeries)

  val series: ObservableBuffer[JFXSeries[Number, Number]] = ObservableBuffer()

  seriesMap.onChange { (_, change) =>
    change match
      case ObservableMap.Add(speciesId, s) => series.addOne(s)
       
  }

  session.speciesFitness.onChange { (_, change) =>
    change match
      case ObservableMap.Add(generation, fitness) => fitness.foreach { (species, score) =>
        val serie = seriesMap.getOrElseUpdate(species, Series[Number, Number](s"$species", ObservableBuffer()))
        serie.data() += Data(generation, score)
      }

  }
}
