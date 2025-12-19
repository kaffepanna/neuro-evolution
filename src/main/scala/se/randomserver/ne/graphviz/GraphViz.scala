package se.randomserver.ne.graphviz

import cats.{Id, ~>}
import cats.arrow.FunctionK

import scala.collection.mutable


object GraphViz {

  case class Node(name: String, attributes: Map[String, String] = Map.empty)
  case class Edge(from: String, to: String, attributes: Map[String, String] = Map.empty)

  enum GrammarA[A]:
    case GGraph(g: Grammar[Unit]) extends GrammarA[Unit]
    case GDiGraph(g: Grammar[Unit]) extends GrammarA[Unit]
    case GSubGraph(name: String, g: Grammar[A]) extends GrammarA[A]
    case GNode(name: String, attributes: Seq[(String, String)])             extends GrammarA[Node]
    case GEdge(from: String, to: String, attributes: Seq[(String, String)]) extends GrammarA[Edge]


  import cats.free.Free
  import cats.free.Free.liftF

  type Grammar[A] = Free[GrammarA, A]

  def digraph(g: Grammar[Unit]): Grammar[Unit] =
    liftF[GrammarA, Unit](GrammarA.GDiGraph(g))

  def graph(g: Grammar[Unit]): Grammar[Unit] =
    liftF[GrammarA, Unit](GrammarA.GGraph(g))

  def subgraph[A](name: String)(g: Grammar[A]): Grammar[A] =
    liftF[GrammarA, A](GrammarA.GSubGraph(name, g))

  def node(name: String, attributes: (String, String)*): Grammar[Node] =
    liftF[GrammarA, Node](GrammarA.GNode(name, attributes))

  def edge_(n1: String, n2: String, attributes: (String, String)*): Grammar[Edge] =
    liftF[GrammarA, Edge](GrammarA.GEdge(n1, n2, attributes))
  def edge(n1: Node, n2: Node, attributes: Seq[(String, String)] = Seq.empty): Grammar[Edge] =
    liftF[GrammarA, Edge](GrammarA.GEdge(n1.name, n2.name, attributes))
}

