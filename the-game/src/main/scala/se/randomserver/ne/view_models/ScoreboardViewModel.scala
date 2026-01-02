package se.randomserver.ne.view_models

import scalafx.Includes.{*, given}
import scalafx.collections.ObservableBuffer
import se.randomserver.ne.the_game.Game.IndividualState

object ScoreboardViewModel {
  case class ScoreRow(id: Int, team: Int, score: Double, alive: Boolean)
}

class ScoreboardViewModel(session: SessionViewModel) {
  import ScoreboardViewModel.ScoreRow

  val scoreRows = ObservableBuffer[ScoreRow]()

  session.currentGameState.onChange { (_, _, ostate) => ostate match
    case Some(state) => scoreRows.setAll(
      state.individuals.map {
        case _ -> IndividualState(id, team, _, score, alive, _) => ScoreRow(id, team, score, alive)
      }.toSeq: _*
    )
    case None =>
      scoreRows.clear()
  }
}
