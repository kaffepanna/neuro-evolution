package se.randomserver.ne.the_game

import se.randomserver.ne.the_game.Game.Cell
import scala.util.Random

object Utils {
  def splitEvenly[A](vec: Vector[A], parts: Int): Vector[Vector[A]] = {
    require(parts > 0, "parts must be > 0")

    val total = vec.length
    val base  = total / parts
    val extra = total % parts

    var offset = 0

    Vector.tabulate(parts) { i =>
      val size = base + (if (i < extra) 1 else 0)
      val chunk = vec.slice(offset, offset + size)
      offset += size
      chunk
    }
  }

  extension (grid: Vector[Vector[Cell]]) {
    def rows: Int = grid.size
    def cols: Int = grid.headOption.map(_.size).getOrElse(0)

    def allPositions = for {
      r <- 0 until rows
      c <- 0 until cols
    } yield (r, c)

    def occupied(r: Int, c: Int): Boolean =
      if (r < 0 || rows <= r || c < 0 || cols <= c) true
      else if (grid(r)(c) == Cell.Individual) true
      else if (grid(r)(c) == Cell.Obstacle) true
      else if (grid(r)(c) == Cell.Food) true
      else
        List((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1))
          .filter { case (i, j) => i >= 0 && i < rows && j >= 0 && j < cols }
          .map { case (rr, cc) => grid(rr)(cc) }
          .exists { cell => 
            cell match
              case Cell.Individual(_, _) => true
              case _ => false
          }

    def place(id: Game.Id, teamId: Game.TeamId)(using rand: Random): ((Int, Int), Vector[Vector[Cell]]) = {
      val freePos = allPositions.filterNot { case (r, c) => occupied(r, c) }
      if (freePos.isEmpty) throw new IllegalStateException("No free positions to place on grid")

      val (r,c) = freePos(rand.between(0, freePos.size))
    (r, c) -> grid.updated(r, grid(r).updated(c, Cell.Individual(id, teamId)))
    }
  }
}

