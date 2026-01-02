package se.randomserver.ne.ui

import scalafx.scene.layout.Pane
import scalafx.scene.canvas.Canvas
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import se.randomserver.ne.the_game.Game
import se.randomserver.ne.view_models.GridViewModel

class GridPane(gridViewModel: GridViewModel) extends Pane {
  val canvas = new Canvas()

  prefWidth = 800
  prefHeight = 400
  maxHeight = Double.MaxValue
  maxWidth = Double.MaxValue

  children.add(canvas)

  canvas.width <== width
  canvas.height <== height

  gridViewModel.gameState.onChange {
    gridViewModel.gameState.value.foreach(redraw)
  }

  canvas.width.onChange {
      gridViewModel.gameState.value.foreach(redraw)
  }

  canvas.height.onChange {
    gridViewModel.gameState.value.foreach(redraw)
  }

  private def redraw(gameState: Game.GameState): Unit = {
    val M = gameState.grid.size
    val N = gameState.grid.head.size
    val gc = canvas.graphicsContext2D
    val width = canvas.width.value
    val height = canvas.height.value
    val cellSize = math.min(width/N, height/M)

    // Compute offsets to center the grid
    val gridWidth = cellSize * N
    val gridHeight = cellSize * M
    val offsetX = (width - gridWidth) / 2
    val offsetY = (height - gridHeight) / 2

    gc.fill = Color.White
    gc.fillRect(0,0,width,height)

    // Draw grid cells
    for (r <- 0 until M; c <- 0 until N) {
      val x = offsetX + c*cellSize
      val y = offsetY + r*cellSize

      gameState.grid(r)(c) match {
        case Game.Cell.Food => gc.fill = Color.Green // food
        case Game.Cell.Obstacle => gc.fill = Color.Black // obstacle
        case _ => gc.fill = Color.White
      }

      gc.fillRect(x, y, cellSize, cellSize)
      gc.stroke = Color.Gray
      gc.strokeRect(x, y, cellSize, cellSize)
    }

    // Draw agents
    gameState.individuals.values.foreach { ind =>
      if (ind.alive) {
        val (r, c) = ind.pos
        val x = offsetX + c*cellSize
        val y = offsetY + r*cellSize
        // gc.fill = ind.team match {
        //   case 0 => Color.Blue
        //   case 1 => Color.Red
        //   case _ => Color.Purple
        // }
        val hue = (ind.team * 137.508) % 360 // golden angle
        gc.fill = Color.hsb(hue, 0.8, 0.85)
        gc.fillRect(x+2, y+2, cellSize-4, cellSize-4) // margin inside cell
      }
    }
  }
}
