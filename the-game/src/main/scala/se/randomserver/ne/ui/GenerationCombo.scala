package se.randomserver.ne.ui

import se.randomserver.ne.view_models.SessionViewModel
import scalafx.scene.control.ComboBox
import scalafx.scene.layout.Pane
import se.randomserver.ne.view_models.GenerationsViewModel
import javafx.scene.control.{ ListCell => J2FXListCell }
import scalafx.Includes._
import scalafx.scene.control.ListCell
import scalafx.scene.control.ListView

class GenerationCombo(model: GenerationsViewModel) extends Pane {
  def generationCell =
    new J2FXListCell[Long] {
      override def updateItem(item: Long, empty: Boolean): Unit = {
        super.updateItem(item, empty)
        setText(if (empty || item == -1) ""
               else s"Generation ${item.toString}")
        
      }
    }
  val comboBox = new ComboBox[Long](model.generationIds) {
    cellFactory = ((_: ListView[Long]) => ListCell[Long](generationCell))
    buttonCell = generationCell

    selectionModel().selectedItemProperty.onChange { (_, _, sel) =>
      model.selectedGenerationId.value = sel
    }

    model.selectedGenerationId.onChange { (_, _, sel) =>
      if (sel == null || sel.doubleValue < 0) selectionModel().clearSelection()
      else selectionModel().select(sel.longValue())
    }
  }

  children += comboBox
}
