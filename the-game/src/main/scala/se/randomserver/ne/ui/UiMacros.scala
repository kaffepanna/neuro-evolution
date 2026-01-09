package se.randomserver.ne.ui

import scala.compiletime.{erasedValue, constValue, summonFrom}
import scala.deriving.Mirror
import scalafx.scene.control.{Label, TextField}
import javafx.util.converter.{IntegerStringConverter, DoubleStringConverter}
import java.util.function.UnaryOperator
import scalafx.scene.control.TextFormatter
import scalafx.scene.control.TextFormatter.Change


object UIMacros {
  inline def summonMirrorOpt[T]: Option[Mirror.ProductOf[T]] =
    summonFrom {
      case m: Mirror.ProductOf[T] => Some(m)
      case _ => None
    }

  inline def summonLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: (h *: t) =>
        constValue[h].toString :: summonLabels[t]
      case _: EmptyTuple =>
        Nil

  inline def buildFieldsWithDefaults[Types <: Tuple](
    labels: List[String],
    values: List[Any],
    prefix: String = ""
  ): Seq[(Label, TextField)] =
    inline erasedValue[Types] match
      case _: (h *: t) =>
        val labelName =
          if prefix.isEmpty then labels.head
          else s"$prefix.${labels.head}"
  
        val value = values.head
  
        val current =
          inline summonMirrorOpt[h] match
            case Some(m) =>
              // Nested case class → recurse
              buildFieldsWithDefaults[
                m.MirroredElemTypes
              ](
                summonLabels[m.MirroredElemLabels],
                value.asInstanceOf[Product].productIterator.toList,
                labelName
              )
  
            case None =>
              // Leaf field
              buildLeafField[h](labelName, value)
  
        current ++ buildFieldsWithDefaults[t](labels.tail, values.tail, prefix)
  
      case _: EmptyTuple =>
        Nil


  inline def buildLeafField[T](label: String, value: Any): Seq[(Label, TextField)] =
    inline erasedValue[T] match
      case _: Int =>
        Seq(
          Label(label) ->
            new TextField {
              textFormatter = Helpers.intFormatter
              text = value.asInstanceOf[Int].toString
            }
        )
  
      case _: Double =>
        Seq(
          Label(label) ->
            new TextField {
              textFormatter = Helpers.doubleFormatter
              text = value.asInstanceOf[Double].toString
            }
        )
  
      case _: String =>
        Seq(
          Label(label) ->
            new TextField {
              text = value.asInstanceOf[String]
            }
        )
  
      case _ =>
        Nil


  def productValues(value: Product): List[Any] =
    value.productIterator.toList

  def readInt(tf: TextField): Int =
  tf.textFormatter().valueProperty().getValue()
    .asInstanceOf[java.lang.Integer]
    .intValue()

  def readDouble(tf: TextField): Double =
    tf.textFormatter().valueProperty().getValue()
      .asInstanceOf[java.lang.Double]
      .doubleValue()
  
  def readString(tf: TextField): String =
    tf.text.value

  inline def readValueCopy[T](instance: T, fields: List[TextField]): (T, List[TextField]) =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        // nested case class
        val elems = instance.asInstanceOf[Product].productIterator.toList
        val (updatedElems, rest) = readTupleCopy[m.MirroredElemTypes](elems, fields)
        (m.fromProduct(Tuple.fromArray(updatedElems.toArray)), rest)
  
      case _ =>
        // leaf field
        inline erasedValue[T] match
          case _: Int =>
            (readInt(fields.head).asInstanceOf[T], fields.tail)
          case _: Double =>
            (readDouble(fields.head).asInstanceOf[T], fields.tail)
          case _: String =>
            (readString(fields.head).asInstanceOf[T], fields.tail)
          case _ =>
            (instance, fields)
    }

  inline def readTupleCopy[Types <: Tuple](elems: List[Any], fields: List[TextField], acc: List[Any] = Nil): (List[Any], List[TextField]) =
    inline erasedValue[Types] match
      case _: (h *: t) =>
        val (value, rest) = readValueCopy[h](elems.head.asInstanceOf[h], fields)
        readTupleCopy[t](elems.tail, rest, acc :+ value)
      case _: EmptyTuple =>
        (acc, fields)
  

  inline def readFormWithDefaults[T](instance: T, fields: Seq[(Label, TextField)])(using m: Mirror.ProductOf[T]): T =
    val onlyFields = fields.map(_._2).toList
    readValueCopy[T](instance, onlyFields)._1


  inline def textFieldsFor[T](value: T)(using m: Mirror.ProductOf[T]): Seq[(Label,TextField)] =
    val labels = summonLabels[m.MirroredElemLabels]
    val values = productValues(value.asInstanceOf[Product])
    buildFieldsWithDefaults[m.MirroredElemTypes](labels, values)
}


