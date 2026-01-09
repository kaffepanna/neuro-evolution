package se.randomserver.ne.ui

import scalafx.Includes.{*, given}
import scalafx.scene.canvas.Canvas
import scalafx.scene.canvas.GraphicsContext
import scalafx.scene.layout.Pane
import scalafx.beans.property.ObjectProperty
import scalafx.scene.paint.Color
import scalafx.application.Platform
import se.randomserver.ne.ui.GridPane.CellStyle

object GridPane {
  case class CellStyle(color: Color, padding: Double)
  object CellStyle {
    def default = CellStyle(Color.White, 0)
  }
}

class GridPane[T](gridProperty: ObjectProperty[Vector[Vector[T]]]) extends Pane {
  val stylePickerProperty = ObjectProperty[(Int, Int, T) => CellStyle]((_, _, _) => CellStyle.default)
  val onCellSelectProperty = ObjectProperty[(Int, Int, T) => Unit]((_, _, _) => ())
  val cellOverlayProperty = ObjectProperty[(Int, Int, T, (Double, Double), Double, GraphicsContext) => Unit]((_, _, _, _, _, _) => ())
  val canvas = new Canvas()

  prefWidth = 800
  prefHeight = 400
  maxHeight = Double.MaxValue
  maxWidth = Double.MaxValue

  children.add(canvas)

  canvas.width <== width
  canvas.height <== height

  gridProperty.onChange {
    redraw
  }
  canvas.width.onChange { redraw }
  canvas.height.onChange { redraw }
  stylePickerProperty.onChange { redraw }

  def stylePicker_=(fn: (Int, Int, T) => CellStyle): Unit = stylePickerProperty.value = fn
  def stylePicker: (Int, Int, T) => CellStyle = stylePickerProperty()

  def onCellSelect_=(fn: (Int, Int, T) => Unit) = onCellSelectProperty.value = fn
  def onCellSelect = onCellSelectProperty.value

  def cellOverlay_=(fn: (Int, Int, T, (Double, Double), Double, GraphicsContext) => Unit): Unit = cellOverlayProperty.value = fn
  def cellOverlay = cellOverlayProperty.value

  canvas.onMousePressed = { e =>
    if (gridProperty.value != null) {
      val grid = gridProperty()
      val w = canvas.width()
      val h = canvas.height()
      val rows = grid.size
      val cols = grid.head.size
      val cellSize = math.min(w / cols, h / rows)

      val gridWidth = cellSize * cols
      val gridHeight = cellSize * rows
      
      val xOffset = (w - gridWidth) / 2
      val yOffset = (h - gridHeight) / 2
      val c = ((e.getX - xOffset) / cellSize).toInt
      val r = ((e.getY - yOffset) / cellSize).toInt
 
      if (r >= 0 && r < rows && c >=0 && c < cols)
        onCellSelect(r, c, grid(r)(c))
    }
  }
  
  private def redraw: Unit = {
    if (gridProperty.value == null)
      return
    val grid = gridProperty()
    val gc = canvas.graphicsContext2D 
    val w = canvas.width()
    val h = canvas.height()

    val rows = grid.size
    val cols = grid.head.size

    val cellSize = math.min(w / cols, h / rows)

    val gridWidth = cellSize * cols
    val gridHeight = cellSize * rows

    val xOffset = (w - gridWidth) / 2
    val yOffset = (h - gridHeight) / 2

    gc.fill = Color.WhiteSmoke
    gc.fillRect(0, 0, w, h)
    for {
      r <- 0 until rows
      c <- 0 until cols
    } {
      val x = xOffset + c * cellSize
      val y = yOffset + r * cellSize

      gc.stroke = Color.Gray
      val CellStyle(color, padding) = stylePicker(r, c, grid(r)(c))
      gc.fill = color
      gc.strokeRect(x, y, cellSize, cellSize)
      gc.fillRect(x + padding/2 , y + padding/2, cellSize-padding, cellSize-padding)
      cellOverlay(r, c, grid(r)(c), (x, y), cellSize, gc)
    }
  }
}
