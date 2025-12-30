package se.randomserver.ne.the_game

import scala.util.Random

object Game {
  type Id = Long
  type TeamId = Long
  type Pos = (Int, Int)

  enum Cell {
    case Empty
    case Individual(id: Id, team: TeamId)
    case Obstacle
    case Food
  }

  case class IndividualState(
    id: Id,
    team: TeamId,
    pos: Pos,
    score: Double = 0.0d,
    alive: Boolean = true,
    visited: Set[Pos] = Set.empty
  )

  enum Action {
    case Nothing, North, East, West, South
  }

  enum Resolution {
    case Move(to: Pos)
    case Stay(where: Pos)
    case Die
  }

  case class GameState(
    grid: Vector[Vector[Cell]],
    individuals: Map[Id, IndividualState],
  )

  object GameState {
    def random(rows: Int, cols: Int, inds: Set[IndividualState]): GameState =
      val r = new Random()
      val foods = Vector.fill(Math.sqrt(rows.toDouble*cols.toDouble).toInt)((r.between(0, rows), r.between(0, cols)))
      val grid = foods.foldLeft(Vector.fill(rows, cols)(Cell.Empty)) { (g, pos) =>
        g.updated(pos._1, g(pos._1).updated(pos._2, Cell.Food))
      }
      GameState(
        grid,
        inds.map(i => i.id -> i).toMap
      )
  }

  def vision(
      state: GameState,
      id: Game.Id,
      radius: Int
  ): Vector[Vector[Game.Cell]] = {
    val ind = state.individuals(id)
    val (r0, c0) = ind.pos
    val size = 2 * radius + 1

    Vector.tabulate(size, size) { (i, j) =>
      val pos = (r0 + i - radius, c0 + j - radius)
      cellAt(state.grid, pos).getOrElse(Game.Cell.Obstacle)
    }
  }


  def computeIntents(
    state: GameState,
    actions: Map[Id, Action]
  ): Map[Id, Pos] = {

    state.individuals.filter(_._2.alive).map { case (id, ind) =>
      val action = actions.getOrElse(id, Action.Nothing)
      val target = move(ind.pos, action)
      id -> target
    }
  }

  def move(pos: Pos, a: Action): Pos = a match {
    case Action.North => (pos._1 - 1, pos._2)
    case Action.South => (pos._1 + 1, pos._2)
    case Action.West  => (pos._1, pos._2 - 1)
    case Action.East  => (pos._1, pos._2 + 1)
    case Action.Nothing => pos
  }

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
      individuals.filter(is => is.alive).map(i => i.pos -> i).toMap

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
    pos: Pos,
    ids: Iterable[Id]
  ): Map[Id, Resolution] = {

    val winner = pickOne(ids)

    ids.map { id =>
      if (Some(id) == winner)
        id -> Resolution.Move(pos)
      else
        id -> Resolution.Stay(pos)
    }.toMap
  }

  def resolveConflicts(
    state: GameState,
    intents: Map[Id, Pos]
  ): Map[Id, Resolution] = {

    val grouped = intents.groupMap(_._2)(_._1)

    grouped.flatMap { case (pos, ids) =>

      val teams = ids.map(id => state.individuals(id).team).toSet

      cellAt(state.grid, pos) match {

        // ⛔ outside or obstacle → nobody moves
        case None | Some(Cell.Obstacle) =>
          ids.map(id => id -> Resolution.Stay(pos))

        // ⚔️ multiple teams arrive at same cell → all die
        case _ if teams.size > 1 =>
          ids.map(id => id -> Resolution.Die)

        // 🍎 food (single team guaranteed here)
        case Some(Cell.Food) =>
          if (ids.size == 1)
            Map(ids.head -> Resolution.Move(pos))
          else
            resolveSameTeamCollision(state, pos, ids)

        // ⬜ empty
        case Some(Cell.Empty | Cell.Individual(_, _)) =>
          if (ids.size == 1)
            Map(ids.head -> Resolution.Move(pos))
          else
            resolveSameTeamCollision(state, pos, ids)
      }
    }
  }

  def resolveAdjacencyCombat(
    state: GameState
  ): Map[Id, Resolution] = {

    val alive =
      state.individuals.filter(_._2.alive)

    val byPos =
      alive.values.map(i => i.pos -> i.id).toMap

    alive.collect {
      case (id, ind) =>
        val adj =
          neighborsPositions(ind.pos)
            .flatMap(byPos.get)
            .map(nid => alive(nid))

        val (allies, enemies) =
          adj.partition(_.team == ind.team)

        if (enemies.size > allies.size)
          id -> Resolution.Die
        else
          id -> Resolution.Stay(ind.pos)
    }
  }

  def applyResolution(
    state: GameState,
    resolutions: Map[Id, Resolution]
  ): GameState = {

    val updatedIndividuals =
      state.individuals.map { case (id, ind) =>
        resolutions.get(id) match {
          case Some(Resolution.Move(pos)) =>
            id -> ind.copy(pos = pos, visited = ind.visited + pos)
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
        case Some(Resolution.Move(pos)) if !oldState.individuals(id).visited.contains(pos) =>
          deltaScores += id -> (deltaScores(id) + 0.2)
        case _ => ()
      }
    }

    // 2️⃣ Food reward
    foodConsumed.foreach { case (id, _) =>
      deltaScores += id -> (deltaScores(id) + 1.0)
    }

    // 3️⃣ Adjacency combat: assign rewards to winners
    // Agents that died in adjacency combat
    val deadByCombat = adjacencyCombatRes.collect {
      case (id, Resolution.Die) => id
    }.toSet

    // For each dead agent, find adjacent allies in oldState and award them
    deadByCombat.foreach { deadId =>
      val dead = oldState.individuals(deadId)
      val neighbors = neighborsPositions(dead.pos)

      neighbors.foreach { pos =>
        oldState.individuals.collect {
          case (id, ind) if ind.alive && ind.team == dead.team && neighbors.contains(ind.pos) =>
            // No points for allies of the dead; skip
            ()
          case (id, ind) if ind.alive && ind.team != dead.team && neighbors.contains(ind.pos) =>
            // Enemy adjacent to dead agent gets a reward
            deltaScores += id -> (deltaScores(id) + 5.0)
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
      case IndividualState(id, _, pos, _, _, _) if cellAt(oldState.grid, pos) == Some(Cell.Food)=> id -> pos
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
