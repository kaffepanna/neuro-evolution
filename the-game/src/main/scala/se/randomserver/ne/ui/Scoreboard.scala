package se.randomserver.ne.ui
import se.randomserver.ne.view_models.ScoreboardViewModel
import scalafx.scene.layout.Pane
import scalafx.scene.control.TableView
import scalafx.beans.property.StringProperty
import scalafx.scene.control.TableColumn
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle
import scalafx.beans.property.ObjectProperty
import scalafx.scene.control.TableCell
import scalafx.Includes._
import scalafx.scene.layout.StackPane

class Scoreboard(model: ScoreboardViewModel) extends StackPane {
  import ScoreboardViewModel.ScoreRow
  import ScalaFxOps.{*, given}
  val tableView = new TableView[ScoreRow](model.scoreRows) {
    maxHeight = Double.MaxValue
    columns ++= Seq(
      new TableColumn[ScoreRow, Color] {
        text = "Team"
        cellValueFactory = { row => ObjectProperty[Color](row.value.team.color) }
        cellFactory = (cell, color) => {
          cell.graphic = Rectangle(8,8, color)
        }
      },
      new TableColumn[ScoreRow, String] {
        text = "Id"
        cellValueFactory = { row => StringProperty(row.value.id.toString) }
      },

      new TableColumn[ScoreRow, String] {
        text = "Score"
        cellValueFactory = { cell => StringProperty(String.format("%.2f", cell.value.score))}
      }
    )
  }
  maxHeight = Double.MaxValue
  children += tableView
}
