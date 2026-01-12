package se.randomserver.ne.ui

import scalafx.Includes.*
import scalafx.beans.property.ObjectProperty
import scalafx.geometry.Insets
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.ButtonType
import scalafx.scene.control.Dialog
import scalafx.scene.control.TextField
import scalafx.scene.layout.GridPane as FXGridPane
import scalafx.scene.layout.HBox
import scalafx.scene.layout.Region
import scalafx.scene.paint.Color
import se.randomserver.ne.GameEvolution.GameEvolutionEnv
import se.randomserver.ne.the_game.Game
import se.randomserver.ne.the_game.Game.Cell
import se.randomserver.ne.ui.GridPane
import se.randomserver.ne.ui.GridPane.CellStyle

class SessionDialog(env: GameEvolutionEnv) extends Dialog[GameEvolutionEnv] {
  val startButtonType = new ButtonType("Start", ButtonData.OKDone)

  this.dialogPane().buttonTypes = Seq(startButtonType, ButtonType.Cancel)

  val gridProperty = ObjectProperty[Vector[Vector[Game.Cell]]](env.grid)

  val gridView = GridPane[Game.Cell](gridProperty)
  resizable = true

  gridView.stylePicker = { (_, _, cell) =>
    cell match
      case Cell.Empty                => CellStyle(Color.WhiteSmoke, 0)
      case Cell.Individual(id, team) => CellStyle(Color.Red, 4)
      case Cell.Obstacle             =>
        CellStyle(Color.Black, 4)
      case Cell.Food => CellStyle(Color.Green, 0)
  }

  gridView.onCellSelect = { (r, c, cell) =>
    val grid = gridProperty.value
    println(s"Cell selected ($r, $c)")
    cell match
      case Cell.Empty =>
        println("empty cell")
        gridProperty.value = grid.updated(r, grid(r).updated(c, Cell.Obstacle))
      case Cell.Obstacle =>
        println("obstacle")
        gridProperty.value = grid.updated(r, grid(r).updated(c, Cell.Empty))
      case _ =>
  }

  val fields = UIMacros.textFieldsFor[GameEvolutionEnv](env)

  val grid = new FXGridPane() {
    fields.zipWithIndex.foreach { case ((label, tf), i) =>
      add(label, 0, i)
      add(tf, 1, i)
    }
    padding = Insets(50, 20, 20, 20)
  }

  this.dialogPane().content = new HBox { hbox =>
    children = Seq(gridView, grid)
  }

  this.dialogPane().setMinHeight(Region.USE_COMPUTED_SIZE)

  resultConverter = dialogButton =>
    dialogButton match
      case `startButtonType` =>
        val result =
          UIMacros.readFormWithDefaults[GameEvolutionEnv](env, fields)
        result.copy(grid = gridProperty.value)
      case _ => null
}
