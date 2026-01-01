package se.randomserver.ne.the_game

import scala.util.Random

object RandomPlacer {
  import Game.{TeamId, Id, Pos}
  def placeIds(
      ids: Set[(Id, TeamId)],
      n: Int,
      m: Int
  ): Option[Map[Id, Pos]] = {

    val rand = new Random()

    // All positions on the grid
    val allPositions = for {
        x <- 0 until n
        y <- 0 until m
    } yield (x, y)

    // adjacency helper
    def neighbors(pos: Pos): Set[Pos] = {
        val (x, y) = pos
        Set((x-1, y), (x+1, y), (x, y-1), (x, y+1))
        .filter { case (i,j) => i >= 0 && i < n && j >= 0 && j < m }
    }

    def helper(
        remaining: List[(Id, TeamId)],
        occupied: Map[Pos, TeamId],
        result: Map[Id, Pos]
    ): Option[Map[Id, Pos]] = remaining match {
        case Nil => Some(result)
        case (id, team) :: tail =>
        val freePositions = allPositions
            .filterNot(occupied.contains)
            .filter { pos =>
            neighbors(pos).forall { nb =>
                occupied.get(nb).forall(_ == team)
            }
            }

        if (freePositions.isEmpty) None
        else {
            // pick random position
            val pos = freePositions(rand.nextInt(freePositions.size))
            val newOccupied = occupied + (pos -> team)
            helper(tail, newOccupied, result + (id -> pos))
        }
    }

    // shuffle input to randomize
    val shuffled = rand.shuffle(ids.toList)
    helper(shuffled, Map.empty, Map.empty)
    }

  def placeIdsWithObstacles(
      ids: Set[(Id, TeamId)],
      n: Int,
      m: Int,
      obstacles: Set[Pos]
  ): Option[Map[Id, Pos]] = {

    val rand = new Random()

    // All positions that are not obstacles
    val allPositions = for {
      x <- 0 until n
      y <- 0 until m
      if !obstacles.contains((x, y))
    } yield (x, y)

    // Get orthogonal neighbors
    def neighbors(pos: Pos): Set[Pos] = {
      val (x, y) = pos
      Set((x-1, y), (x+1, y), (x, y-1), (x, y+1))
        .filter { case (i, j) => i >= 0 && i < n && j >= 0 && j < m }
    }

    // Recursive helper to place IDs
    def helper(
        remaining: List[(Id, TeamId)],
        occupied: Map[Pos, TeamId],
        result: Map[Id, Pos]
    ): Option[Map[Id, Pos]] = remaining match {
      case Nil => Some(result)
      case (id, team) :: tail =>
        val freePositions = allPositions
          .filterNot(occupied.contains)
          .filter { pos =>
            neighbors(pos).forall { nb =>
              occupied.get(nb).forall(_ == team)
            }
          }

        if (freePositions.isEmpty) None
        else {
          // pick random position
          val pos = freePositions(rand.nextInt(freePositions.size))
          val newOccupied = occupied + (pos -> team)
          helper(tail, newOccupied, result + (id -> pos))
        }
    }

    val shuffled = rand.shuffle(ids.toList)
    helper(shuffled, Map.empty, Map.empty)
  }
}
