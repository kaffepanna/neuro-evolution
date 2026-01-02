package se.randomserver.ne.view_models

import scalafx.beans.property.ObjectProperty
import se.randomserver.ne.the_game.Game.GameState

class GridViewModel(session: SessionViewModel) {
  val gameState = ObjectProperty[Option[GameState]](None)

  gameState <== session.currentGameState
}
