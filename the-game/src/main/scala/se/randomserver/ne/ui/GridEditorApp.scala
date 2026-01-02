package se.randomserver.ne.ui

import scalafx.application.JFXApp3
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color
import scalafx.scene.control.Button
import scalafx.stage.FileChooser
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.HBox
import scalafx.geometry.Insets
import scalafx.geometry.Pos
import scalafx.scene.Scene
import java.io.File
import java.nio.file.Files
import java.nio.file.Path
import scala.util.Try

object GridEditorApp extends JFXApp3 {
  final case class GridModel(rows: Int, cols: Int) {
    private val data = Array.fill(rows, cols)(0)

    def toggle(r: Int, c: Int): Unit =
      data(r)(c) = if (data(r)(c) == 0) 1 else 0

    def toVector: Vector[Vector[Int]] =
      data.map(_.toVector).toVector
  }

  def drawGrid(
    canvas: Canvas,
    model: GridModel
  ): Unit = {
    val gc = canvas.graphicsContext2D
    val w = canvas.width.value
    val h = canvas.height.value

    val cellSize = math.min(w / model.cols, h / model.rows)

    val xOffset = (w - model.cols * cellSize) / 2
    val yOffset = (h - model.rows * cellSize) / 2

    gc.fill = Color.White
    gc.fillRect(0, 0, w, h)

    for {
      r <- 0 until model.rows
      c <- 0 until model.cols
    } {
      val x = xOffset + c * cellSize
      val y = yOffset + r * cellSize

      gc.fill =
        if (model.toVector(r)(c) == 1) Color.Black
        else Color.White

      gc.fillRect(x, y, cellSize, cellSize)
      gc.stroke = Color.Gray
      gc.strokeRect(x, y, cellSize, cellSize)
    }
  }

  def save(grid: Vector[Vector[Int]], file: File): Unit = {
   val content =
      grid.map(_.mkString(" ")).mkString("\n")
    Files.writeString(file.toPath, content)
  }
  def loadGrid(path: Path): Either[String, Vector[Vector[Int]]] = {
    import scala.jdk.CollectionConverters.*
    Try {
      val lines =
        Files.readAllLines(path).asScala.toVector

      val grid =
        lines
          .filter(_.trim.nonEmpty)
          .map { line =>
            line.trim
              .split("\\s+")
              .toVector
              .map(_.toInt)
          }

      // Sanity check: rectangular grid
      val widths = grid.map(_.size).distinct
      if (widths.size > 1)
        throw new IllegalArgumentException("Non-rectangular grid")

      grid
    }.toEither.left.map(_.getMessage)
  }

  override def start(): Unit = {
    val rows = 30
    val cols = 30
    val model = GridModel(rows, cols)

    val canvas = new Canvas(600, 600)

    canvas.onMousePressed = e => {
      val cellSize = math.min(
        canvas.width.value / cols,
        canvas.height.value / rows
      )

      val xOffset = (canvas.width.value - cols * cellSize) / 2
      val yOffset = (canvas.height.value - rows * cellSize) / 2

      val c = ((e.getX - xOffset) / cellSize).toInt
      val r = ((e.getY - yOffset) / cellSize).toInt

      if (r >= 0 && r < rows && c >= 0 && c < cols) {
        model.toggle(r, c)
        drawGrid(canvas, model)
      }
    }


    canvas.width.onChange { (_, _, _) =>
      drawGrid(canvas, model)
    }
    canvas.height.onChange { (_, _, _) =>
      drawGrid(canvas, model)
    }

    val saveButton = new Button("Save") {
      onAction = _ => {
        val chooser = new FileChooser
        val file = chooser.showSaveDialog(stage)
        if (file != null)
          save(model.toVector, file)
      }
    }

    val rootPane = new BorderPane {
      center = new StackPane {
        children += canvas
      }
      bottom = new HBox(10, saveButton) {
        alignment = Pos.Center
        padding = Insets(10)
      }
    }

    canvas.width <== rootPane.width
    canvas.height <== rootPane.height - 50

    drawGrid(canvas, model)

    stage = new JFXApp3.PrimaryStage {
      title = "Grid Editor"
      scene = new Scene(700, 750) {
        root = rootPane
      }
    }
  }
}
