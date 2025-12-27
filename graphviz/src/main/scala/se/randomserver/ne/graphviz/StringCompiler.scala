package se.randomserver.ne.graphviz

import GraphViz._

import cats.data.Writer
import cats.{Id, ~>}
import cats.syntax.all.{*,given}

object StringCompiler:
  type GraphVizState[A] = Writer[String, A]

  def compiler(level: Int = 0): GrammarA ~> GraphVizState = new (GrammarA ~> GraphVizState) {

    private def writeAttributes(attributes: Seq[(String, String)]): Writer[String, Unit] =
      if attributes.isEmpty then Writer.tell("\n")
                            else Writer.tell(
                              " [" ++ attributes
                                .map(a => s"${a._1} = \"${a._2}\"")
                                .mkString(";") ++ "]\n"
                            )

    override def apply[A](fa: GrammarA[A]): GraphVizState[A] = fa match
      case GrammarA.GGraph(g) => for {
        _ <- Writer.tell("\t" * level ++ "graph {\n")
        _ <- g.foldMap(compiler(level + 1))
        _ <- Writer.tell("\t" * level ++ "}\n")
      } yield ()

      case GrammarA.GDiGraph(g) => for {
        _ <- Writer.tell("\t" * level ++ "digraph {\n")
        _ <- g.foldMap(compiler(level + 1))
        _ <- Writer.tell("\t" * level ++ "}\n")
      } yield ()

      case GrammarA.GSubGraph(name, g) => for {
        _ <- Writer.tell("\t" * level ++ s"subgraph $name {\n")
        o <- g.foldMap(compiler(level + 1))
        _ <- Writer.tell("\t" * level ++ "}\n")
      } yield o

      case GrammarA.GNode(name, attributes) => for {
        _ <- Writer.tell("\t" * level ++ s"$name")
        _ <- writeAttributes(attributes)
      } yield Node(name, Map.from(attributes))

      case GrammarA.GEdge(from, to, attributes) => for {
        _ <- Writer.tell("\t" * level ++ s"$from -> $to")
        _ <- writeAttributes(attributes)
      } yield Edge(from, to, Map.from(attributes))

      case GrammarA.GAttribute(key, value) => for {
        _ <- Writer.tell("\t" * level ++ s"$key=$value\n")
      } yield Attribute(key, value)
  }
end StringCompiler

