package se.randomserver.ne.ui

import scalafx.scene.layout.Pane
import scalafx.scene.canvas.Canvas
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import se.randomserver.ne.the_game.Game
import se.randomserver.ne.view_models.GameReplayViewModel
import se.randomserver.ne.ui.ScalaFxOps.color
import se.randomserver.ne.ui.GridPane.CellStyle
import se.randomserver.ne.the_game.Game.Heading
import se.randomserver.ne.the_game.Game.Cell

import scalafx.scene.layout.Region
import scalafx.scene.layout.Priority

class GameReplay(gridViewModel: GameReplayViewModel)
    extends GridPane[Game.Cell](gridViewModel.gridProperty) {
  stylePicker = { (r, c, cell) =>
    cell match
      case Cell.Empty                => CellStyle(Color.WhiteSmoke, 0)
      case Cell.Individual(id, team) => CellStyle(team.color, 4)
      case Cell.Obstacle             => CellStyle(Color.Black, 0)
      case Cell.Food                 => CellStyle(Color.Green, 0)
  }

  this.cellOverlay = { (row, col, cell, xy, cellSize, gc) =>
    cell match
      case Cell.Individual(id, team) =>
        gridViewModel.gameState.value.foreach { gameState =>
          val (x, y) = xy
          val ind = gameState.individuals(id)
          val cX = x + cellSize / 2
          val cY = y + cellSize / 2
          gc.setStroke(Color.Black)
          ind.pose.heading match
            case Heading.North =>
              gc.strokeLine(cX, cY, cX, cY - cellSize / 2)
            case Heading.East =>
              gc.strokeLine(cX, cY, cX + cellSize / 2, cY)
            case Heading.West =>
              gc.strokeLine(cX, cY, cX - cellSize / 2, cY)
            case Heading.South =>
              gc.strokeLine(cX, cY, cX, cY + cellSize / 2)
        }
      case _ =>
  }
}
