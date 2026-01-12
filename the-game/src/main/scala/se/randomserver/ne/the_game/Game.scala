package se.randomserver.ne.the_game
import scala.util.Random

import Utils.*

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
      individuals: Map[Id, IndividualState]
  )

  object GameState {

    def random(
        grid: Vector[Vector[Cell]],
        inds: Set[(Id, TeamId)]
    ): GameState = {
      given rnd: Random = new Random()

      val (indStates, grid2) =
        inds.foldLeft(Map.empty[Int, IndividualState], grid) {
          case ((is, g), (id, teamId)) =>
            val (pos, gg) = g.place(id, teamId)
            val ind = IndividualState(
              id,
              teamId,
              Pose(pos, Heading.values(rnd.between(0, Heading.values.size)))
            )
            is.updated(id, ind) -> gg
        }

      GameState(
        grid2,
        indStates
      )
    }
  }

  def vision(
      state: GameState,
      id: Game.Id,
      radius: Int
  ): Vector[Vector[Game.Cell]] = {
    val indO = state.individuals.get(id)
    if (indO.isEmpty) {
      println(s"Could not find ind $id in ${state.individuals.keySet}")
    }
    val ind = indO.get
    val Pose((r0, c0), heading) = ind.pose
    val size = 2 * radius + 1

    val base = Vector.tabulate(size, size) { (i, j) =>
      val pos = (r0 + i - radius, c0 + j - radius)
      state.grid.cellAt(pos)
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

  def move(pos: Pose, a: Action): Pose = {
    val Pose((r, c), heading) = pos
    a match
      case Action.Forward =>
        heading match
          case Heading.North => Pose((r - 1, c), heading)
          case Heading.East  => Pose((r, c + 1), heading)
          case Heading.West  => Pose((r, c - 1), heading)
          case Heading.South => Pose((r + 1, c), heading)

      case Action.TurnLeft =>
        heading match
          case Heading.North => Pose((r, c), Heading.West)
          case Heading.East  => Pose((r, c), Heading.North)
          case Heading.West  => Pose((r, c), Heading.South)
          case Heading.South => Pose((r, c), Heading.East)

      case Action.TurnRight =>
        heading match
          case Heading.North => Pose((r, c), Heading.East)
          case Heading.East  => Pose((r, c), Heading.South)
          case Heading.West  => Pose((r, c), Heading.North)
          case Heading.South => Pose((r, c), Heading.West)

      case Action.Nothing => Pose((r, c), heading)
  }

  def resolve(
      state: GameState,
      intents: Map[Id, Resolution]
  ): Map[Id, Resolution] = {

    val deaths = intents.filter(_._2.isInstanceOf[Resolution.Die.type])
    val grouped: Map[
      (Int, Int),
      scala.collection.immutable.Iterable[(Int, Resolution)]
    ] = intents.toList
      .collect {
        case v @ (_, Resolution.Move(pose)) => pose.pos -> v
        case v @ (_, Resolution.Stay(pose)) => pose.pos -> v
      }
      .groupMap(_._1)(_._2)

    val resolved = grouped.flatMap { case (pos, idres) =>
      val idResolutions = idres.toMap
      val teams = idResolutions.map { case (id, _) =>
        state.individuals(id).team
      }.toSet
      if (teams.size > 1)
        idResolutions.map { case (id, _) => id -> Resolution.Die }
      else if (idResolutions.size > 1)
        val stays = idResolutions.filter { case (i, _) =>
          state.individuals(i).pose.pos == pos && state.individuals(i).alive
        }.toMap
        if (stays.size > 1) {
          (idResolutions -- stays.keySet).map { case (id, _) =>
            // others stay at their original position
            id -> Resolution.Stay(state.individuals(id).pose)
          } ++ stays
        } else {
          val winner = idResolutions.head
          (idResolutions - winner._1).map { case (id, _) =>
            id -> Resolution.Stay(state.individuals(id).pose)
          } + winner
        }
      else idResolutions
    }.toMap

    resolved ++ deaths
  }

  def resolveConflicts(
      state: GameState,
      intents: Map[Id, Pose]
  ): Map[Id, Resolution] = {

    val resolutions = intents.map {
      case (id, pose) if state.individuals(id).pose.pos == pose.pos =>
        id -> Resolution.Stay(pose)
      case (id, pose)
          if state.grid
            .cellAt(pose.pos)
            .isInstanceOf[Cell.Obstacle.type | Cell.Individual] =>
        id -> Resolution.Stay(state.individuals(id).pose)
      case (id, pose) => id -> Resolution.Move(pose)
    }

    resolve(state, resolutions)
  }

  def resolveAdjacencyCombat(
      state: GameState
  ): Map[Id, Resolution] = {

    val alive =
      state.individuals.filter(_._2.alive)

    val byPos =
      alive.values.map(i => i.pose.pos -> i.id).toMap

    alive.collect { case (id, ind) =>
      val adj =
        state.grid
          .adjacentPositions(ind.pose.pos)
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
          case Some(Resolution.Stay(pose)) =>
            id -> ind.copy(pose = pose, visited = ind.visited + pose.pos)
          case Some(Resolution.Move(pose)) =>
            id -> ind.copy(pose = pose, visited = ind.visited + pose.pos)
          case Some(Resolution.Die) =>
            id -> ind.copy(alive = false)
          case _ => id -> ind
        }
      }

    state.copy(
      individuals = updatedIndividuals,
      grid = state.grid.rebuild(updatedIndividuals.values)
    )
  }

  def updateScores(
      oldState: GameState,
      newState: GameState,
      moveResolutions: Map[Id, Resolution],
      adjacencyCombatRes: Map[Id, Resolution],
      foodConsumed: Map[Id, Pos] // agent -> food position
  ): GameState = {

    // Initialize delta scores
    var deltaScores = Map.empty[Game.Id, Double].withDefaultValue(0.0)

    // Movement reward
    newState.individuals.foreach { case (id, _) =>
      moveResolutions.get(id) match {
        case Some(Resolution.Move(pose))
            if !oldState.individuals(id).visited.contains(pose.pos) =>
          deltaScores += id -> (deltaScores(id) + 0.2)
        case _ => ()
      }
    }

    // Food reward
    foodConsumed.foreach { case (id, _) =>
      deltaScores += id -> (deltaScores(id) + 5.0)
    }

    // Adjacency combat: assign rewards to winners
    // Agents that died in adjacency combat
    val deadByCombat = adjacencyCombatRes.collect { case (id, Resolution.Die) =>
      id
    }.toSet

    // For each dead agent, find adjacent allies in oldState and award them
    deadByCombat.foreach { deadId =>
      val dead = oldState.individuals(deadId)
      val neighbors = oldState.grid.adjacentPositions(dead.pose.pos)

      neighbors.foreach { _ =>
        oldState.individuals.collect {
          case (_, ind)
              if ind.alive && ind.team == dead.team && neighbors.contains(
                ind.pose.pos
              ) =>
            // No points for allies of the dead; skip
            ()
          case (id, ind)
              if ind.alive && ind.team != dead.team && neighbors.contains(
                ind.pose.pos
              ) =>
            // Enemy adjacent to dead agent gets a reward
            deltaScores += id -> (deltaScores(id) + 10.0)
        }
      }
    }

    // Death penalties
    newState.individuals.foreach { case (id, ind) =>
      if (!ind.alive) {
        val penalty =
          moveResolutions.get(id) match {
            case Some(Resolution.Die)           => -10.0 // collision death
            case _ if deadByCombat.contains(id) => 0.0
            case _                              => 0.0
          }
        deltaScores += id -> (deltaScores(id) + penalty)
      }
    }

    // Apply score updates
    val updatedIndividuals = newState.individuals.map { case (id, ind) =>
      id -> ind.copy(score = ind.score + deltaScores(id))
    }

    newState.copy(individuals = updatedIndividuals)
  }

  def foodConsumed(oldState: GameState, newState: GameState): Map[Id, Pos] = {
    newState.individuals.values
      .filter(_.alive)
      .collect {
        case IndividualState(id, _, pose, _, _, _)
            if oldState.grid.cellAt(pose.pos) == Some(Cell.Food) =>
          id -> pose.pos
      }
      .toMap
  }

  def step(state: GameState, actions: Map[Id, Action]): GameState = {
    val intents = computeIntents(state, actions)
    val moveResolutions = resolveConflicts(state, intents)
    val afterMove = applyResolution(state, moveResolutions)
    val food = foodConsumed(state, afterMove)

    val combatResolutions = resolveAdjacencyCombat(afterMove)
    val afterCombat = applyResolution(afterMove, combatResolutions)

    updateScores(state, afterCombat, moveResolutions, combatResolutions, food)
  }
}
