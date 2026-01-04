package se.randomserver.ne.the_game

import scala.util.Random
import se.randomserver.ne.ui.GridEditorApp
import java.nio.file.Path
import java.nio.file.Paths

object Game {
  type Id = Int
  type TeamId = Int
  type Pos = (Int, Int)
  case class Pose(pos: Pos, heading: Heading)

  enum Cell {
    case Empty
    case Individual(id: Id, team: TeamId)
    case Obstacle
    case Food
  }

  enum Heading {
    case North, East, West, South
  }

  case class IndividualState(
    id: Id,
    team: TeamId,
    pose: Pose,
    score: Double = 0.0d,
    alive: Boolean = true,
    visited: Set[Pos] = Set.empty
  )

  enum Action {
    case Nothing, Forward, TurnLeft, TurnRight
  }

  enum Resolution {
    case Move(to: Pose)
    case Stay(where: Pose)
    case Die
  }

  case class GameState(
    grid: Vector[Vector[Cell]],
    individuals: Map[Id, IndividualState],
  )

  object GameState {
    
    def random(rows: Int, cols: Int, inds: Set[(Id, TeamId)]): GameState =
      val r = new Random()
      val walled = GridEditorApp.loadGrid(Paths.get("/home/patrik/sources/neuro-evolution/30x30.grid")).right.get.map { row =>
        row.map {
          case 1 => Cell.Obstacle
          case _ => Cell.Empty
        }
      }
      val obstacles = walled.zipWithIndex.flatMap { case (rows, r) => rows.zipWithIndex.collect { case (Cell.Obstacle, c) => (r, c)} }.toSet
      val foods = List.fill(Math.sqrt(rows.toDouble*cols.toDouble).toInt)(2).zipWithIndex.map(_.swap).toSet
      val foodPlaced = RandomPlacer.placeIdsWithObstacles(foods, rows,cols, obstacles).get.values.toSet

      val placements: Map[Id, Pos] = RandomPlacer.placeIdsWithObstacles(inds, rows, cols, obstacles ++ foods) match
        case Some(value) => value
        case None => throw IllegalStateException("cannot place individuals")
      
      val grid2 = foodPlaced.foldLeft(walled) { (g, pos) =>
        g.updated(pos._1, g(pos._1).updated(pos._2, Cell.Food))
      }

      GameState(
        grid2,
        placements.map { case (id, pos) => 
          val team = inds.find(_._1 == id).get._2
          id -> IndividualState(id, team, Pose(pos, Heading.values(r.between(0, Heading.values.size - 1))))
        }
      )
  }

  def vision(
      state: GameState,
      id: Game.Id,
      radius: Int
  ): Vector[Vector[Game.Cell]] = {
    val ind = state.individuals(id)
    val pose @ Pose((r0, c0), heading) = ind.pose
    val size = 2 * radius + 1

    val base = Vector.tabulate(size, size) { (i, j) =>
      val pos = (r0 + i - radius, c0 + j - radius)
      cellAt(state.grid, pos).getOrElse(Game.Cell.Obstacle)
    }
    heading match {
      case Heading.North => base
      case Heading.East  => base.transpose.map(_.reverse)
      case Heading.South => base.transpose.reverse
      case Heading.West  => base.reverse.map(_.reverse)
    }
  }


  def computeIntents(
    state: GameState,
    actions: Map[Id, Action]
  ): Map[Id, Pose] = {

    state.individuals.filter(_._2.alive).map { case (id, ind) =>
      val action = actions.getOrElse(id, Action.Nothing)
      val target = move(ind.pose, action)
      id -> target
    }
  }

  def move(pos: Pose , a: Action): Pose = 
    val Pose((r, c), heading) = pos
    a match
      case Action.Forward  => heading match
        case Heading.North =>  Pose((r - 1 , c    ), heading)
        case Heading.East  =>  Pose((r     , c + 1), heading)
        case Heading.West  =>  Pose((r     , c - 1), heading)
        case Heading.South =>  Pose((r + 1 , c    ), heading)
    
      case Action.TurnLeft => heading match
        case Heading.North => Pose((r, c), Heading.West)
        case Heading.East  => Pose((r, c), Heading.North)
        case Heading.West  => Pose((r, c), Heading.South)
        case Heading.South => Pose((r, c), Heading.East)
      
      case Action.TurnRight => heading match
        case Heading.North => Pose((r, c), Heading.East)
        case Heading.East  => Pose((r, c), Heading.South)
        case Heading.West  => Pose((r, c), Heading.North)
        case Heading.South => Pose((r, c), Heading.West)
      
      case Action.Nothing   => Pose((r, c), heading)


  def inBounds(grid: Vector[Vector[Game.Cell]], pos: Game.Pos): Boolean = {
    val (r, c) = pos
    r >= 0 &&
    c >= 0 &&
    r < grid.length &&
    c < grid.head.length
  }

  def cellAt(
    grid: Vector[Vector[Game.Cell]],
    pos: Game.Pos
  ): Option[Game.Cell] =
    if (inBounds(grid, pos))
      Some(grid(pos._1)(pos._2))
    else
      None

  def pickOne(ids: Iterable[Id]): Option[Id] =
    ids.minOption

  def neighborsPositions(pos: Pos): Set[Pos] =
    Set(
      (pos._1 - 1, pos._2),
      (pos._1 + 1, pos._2),
      (pos._1, pos._2 - 1),
      (pos._1, pos._2 + 1)
    )

  def rebuildGrid(
    oldGrid: Vector[Vector[Cell]],
    individuals: Iterable[IndividualState]
  ): Vector[Vector[Cell]] = {

    val height = oldGrid.length
    val width  = oldGrid.head.length

    // Map positions → individual
    val individualsByPos: Map[(Int, Int), IndividualState] =
      individuals.filter(is => is.alive).map(i => i.pose.pos -> i).toMap

    Vector.tabulate(height, width) { (r, c) =>
      individualsByPos.get((r, c)) match {
        case Some(ind) =>
          Cell.Individual(ind.id, ind.team)

        case None =>
          oldGrid(r)(c) match {
            case Cell.Individual(_, _) =>
              Cell.Empty                    // clear old individuals
            case other =>
              other                         // Obstacle / Food / Empty
          }
      }
    }
  }

  def resolveSameTeamCollision(
    state: GameState,
    pose: Pose,
    ids: Iterable[Id]
  ): Map[Id, Resolution] = {

    val winner = pickOne(ids)

    ids.map { id =>
      if (Some(id) == winner)
        id -> Resolution.Move(pose)
      else
        id -> Resolution.Stay(pose)
    }.toMap
  }

  def resolveConflicts(
    state: GameState,
    intents: Map[Id, Pose]
  ): Map[Id, Resolution] = {

    val grouped = intents.groupMap(_._2)(_._1)

    grouped.flatMap { case (pose, ids) =>

      val teams = ids.map(id => state.individuals(id).team).toSet

      cellAt(state.grid, pose.pos) match {

        // ⛔ outside or obstacle → nobody moves
        case None | Some(Cell.Obstacle) =>
          ids.map(id => id -> Resolution.Stay(pose))

        // ⚔️ multiple teams arrive at same cell → all die
        case Some(Cell.Individual(presentId, presentTeamId)) =>
          resolveSameTeamCollision(state, pose, ids.toSet + presentId)
        case _ if teams.size > 1 =>
          ids.map(id => id -> Resolution.Die)
        case _ =>
          resolveSameTeamCollision(state, pose, ids)
      }
    }
  }

  def resolveAdjacencyCombat(
    state: GameState
  ): Map[Id, Resolution] = {

    val alive =
      state.individuals.filter(_._2.alive)

    val byPos =
      alive.values.map(i => i.pose.pos -> i.id).toMap

    alive.collect {
      case (id, ind) =>
        val adj =
          neighborsPositions(ind.pose.pos)
            .flatMap(byPos.get)
            .map(nid => alive(nid))

        val (allies, enemies) =
          adj.partition(_.team == ind.team)

        if (enemies.size > allies.size)
          id -> Resolution.Die
        else
          id -> Resolution.Stay(ind.pose)
    }
  }

  def applyResolution(
    state: GameState,
    resolutions: Map[Id, Resolution]
  ): GameState = {

    val updatedIndividuals =
      state.individuals.map { case (id, ind) =>
        resolutions.get(id) match {
          case Some(Resolution.Move(pose)) =>
            id -> ind.copy(pose = pose, visited = ind.visited + pose.pos)
          case Some(Resolution.Die) =>
            id -> ind.copy(alive = false)
          case _ => id -> ind
        }
      }

    state.copy(
      individuals = updatedIndividuals,
      grid = rebuildGrid(state.grid, updatedIndividuals.values)
    )
  }

  def updateScores(
      oldState: GameState,
      newState: GameState,
      moveResolutions: Map[Id, Resolution],
      adjacencyCombatRes: Map[Id, Resolution],
      foodConsumed: Map[Id, Pos]  // agent -> food position
  ): GameState = {

    // Initialize delta scores
    var deltaScores = Map.empty[Game.Id, Double].withDefaultValue(0.0)

    // 1️⃣ Movement reward
    newState.individuals.foreach { case (id, ind) =>
      moveResolutions.get(id) match {
        case Some(Resolution.Move(pose)) if !oldState.individuals(id).visited.contains(pose.pos) =>
          deltaScores += id -> (deltaScores(id) + 1.0)
        case _ => ()
      }
    }

    // 2️⃣ Food reward
    foodConsumed.foreach { case (id, _) =>
      deltaScores += id -> (deltaScores(id) + 2.0)
    }

    // 3️⃣ Adjacency combat: assign rewards to winners
    // Agents that died in adjacency combat
    val deadByCombat = adjacencyCombatRes.collect {
      case (id, Resolution.Die) => id
    }.toSet

    // For each dead agent, find adjacent allies in oldState and award them
    deadByCombat.foreach { deadId =>
      val dead = oldState.individuals(deadId)
      val neighbors = neighborsPositions(dead.pose.pos)

      neighbors.foreach { pos =>
        oldState.individuals.collect {
          case (id, ind) if ind.alive && ind.team == dead.team && neighbors.contains(ind.pose.pos) =>
            // No points for allies of the dead; skip
            ()
          case (id, ind) if ind.alive && ind.team != dead.team && neighbors.contains(ind.pose.pos) =>
            // Enemy adjacent to dead agent gets a reward
            deltaScores += id -> (deltaScores(id) + 10.0)
        }
      }
    }

    // 4️⃣ Death penalties
    newState.individuals.foreach { case (id, ind) =>
      if (!ind.alive) {
        val penalty =
          moveResolutions.get(id) match {
            case Some(Resolution.Die) => -10.0   // collision death
            case _ if deadByCombat.contains(id) => -2.0
            case _ => 0.0
          }
        deltaScores += id -> (deltaScores(id) + penalty)
      }
    }

    // 5️⃣ Apply score updates
    val updatedIndividuals = newState.individuals.map { case (id, ind) =>
      id -> ind.copy(score = ind.score + deltaScores(id))
    }

    newState.copy(individuals = updatedIndividuals)
  }

  def foodConsumed(oldState: GameState, newState: GameState): Map[Id, Pos] = {
    newState.individuals.values.filter(_.alive).collect {
      case IndividualState(id, _, pose, _, _, _) if cellAt(oldState.grid, pose.pos) == Some(Cell.Food)=> id -> pose.pos
    }.toMap
  }


  def step(state: GameState, actions: Map[Id, Action]): GameState = {
    val intents         = computeIntents(state, actions)
    val moveResolutions = resolveConflicts(state, intents)
    val afterMove       = applyResolution(state, moveResolutions)
    val food            = foodConsumed(state, afterMove)

    val combatResolutions = resolveAdjacencyCombat(afterMove)
    val afterCombat = applyResolution(afterMove, combatResolutions)

    updateScores(state, afterCombat, moveResolutions, combatResolutions, food)
  }
}
