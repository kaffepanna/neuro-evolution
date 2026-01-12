package se.randomserver.ne.view_models

import scalafx.beans.property.ObjectProperty
import se.randomserver.ne.the_game.Game.GameState
import scalafx.beans.binding.Bindings
import se.randomserver.ne.the_game.Game

class GameReplayViewModel(session: SessionViewModel) {
  val gameState = ObjectProperty[Option[GameState]](None)
  val gridProperty = ObjectProperty[Vector[Vector[Game.Cell]]](
    gameState.value.map(_.grid).getOrElse(null)
  )

  gameState <== session.currentGameState
  gameState.onChange { (_, _, gs) =>
    gridProperty.value = gs.map(_.grid).getOrElse(null)
  }
}
