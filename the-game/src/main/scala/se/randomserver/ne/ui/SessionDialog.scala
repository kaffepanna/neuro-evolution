package se.randomserver.ne.ui

import scalafx.Includes.{*, given}
import scalafx.scene.control.Dialog
import se.randomserver.ne.GameEvolution.GameEvolutionEnv
import scalafx.scene.control.ButtonType
import scalafx.scene.control.ButtonBar.ButtonData
import scalafx.scene.control.TextField
import scalafx.scene.layout.GridPane

class SessionDialog(env: GameEvolutionEnv) extends Dialog[GameEvolutionEnv] {
  val startButtonType = new ButtonType("Start", ButtonData.OKDone)

  this.dialogPane().buttonTypes = Seq(startButtonType, ButtonType.Cancel)
  val teamsInputField = new TextField()

  val fields = UIMacros.textFieldsFor[GameEvolutionEnv](env)

  val grid = new GridPane() {
    fields.zipWithIndex.foreach {
      case ((label, tf), i) => 
        add(label, 0 , i)
        add(tf, 1, i)
    }
  }

  this.dialogPane().content = grid

  resultConverter = dialogButton => dialogButton match
    case `startButtonType` =>
      val result = UIMacros.readFormWithDefaults[GameEvolutionEnv](env, fields)
      result
    case _ => null
}
