package se.randomserver.ne

import se.randomserver.ne.the_game.Game.GameState
import se.randomserver.ne.evolution.Evolution.SpeciesId

final case class Stats(
    generationId: Int,
    game: Vector[GameState],
    speciesFitness: Map[SpeciesId, Double]
)

trait StatsCallback[F[_]] {
  def pushGeneration(stats: Stats): F[Unit]
}

object StatsCallback {
  def apply[F[_]](using cb: StatsCallback[F]): StatsCallback[F] = cb
}
