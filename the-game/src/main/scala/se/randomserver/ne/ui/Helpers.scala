package se.randomserver.ne.ui

import scalafx.Includes.{*, given}
import scalafx.scene.control.TextField
import scalafx.scene.control.TextFormatter
import javafx.util.converter.IntegerStringConverter
import javafx.util.converter.DoubleStringConverter
import java.util.function.UnaryOperator
import javafx.scene.control.TextFormatter.Change

object Helpers {
  def intFormatter: TextFormatter[Integer] =
    new TextFormatter[Integer](
      new javafx.scene.control.TextFormatter[Integer](
        new IntegerStringConverter(),
        0,
        new UnaryOperator[Change] {
          override def apply(change: Change): Change =
            val text = change.getControlNewText
            if (text.matches("-?\\d*")) change else null
        }
      )
    )

  def doubleFormatter: TextFormatter[java.lang.Double] =
    new TextFormatter[java.lang.Double](
      new javafx.scene.control.TextFormatter[java.lang.Double](
        new DoubleStringConverter(),
        0.0,
        new UnaryOperator[Change] {
          override def apply(change: Change): Change =
            val text = change.getControlNewText
            if (text.matches("-?\\d*(\\.\\d*)?")) change else null
        }
      )
    )
}
