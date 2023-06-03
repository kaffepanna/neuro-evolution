package se.randomserver.ne.phenotype

import cats.*
import cats.data.{State, StateT}
import cats.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.GenePoolStateT

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.runtime.stdLibPatches.Predef.assert

case class Individual[W, I <: Int, O <: Int](genome: Genome[W, I, O], nodes: Seq[Option[W]], transfer: W => W) {
  def >>[A](fa: Individual.IndividualState[W, I, O, A]): (Individual[W, I, O], A) =
    Individual.run(this)(fa)
}

object Individual:
  opaque type IndividualState[W, I <: Int, O <: Int, A] = State[Individual[W, I, O], A]
  given[W, I <: Int, O <: Int](using fm: FlatMap[State[Individual[W, I, O], _]]): FlatMap[IndividualState[W, I, O, _]] = fm

  def run[W, I <: Int, O <: Int, A](individual: Individual[W, I, O])(fa: IndividualState[W, I, O, A]): (Individual[W, I, O], A) =
    fa.run(individual).value
  def from[W, I <: Int, O <: Int](genome: Genome[W, I, O], transfer: W => W) = Individual(genome, genome.nodes.map(_ => None), transfer)

  def setInputs[W, I <: Int, O <: Int](data: Seq[W]): IndividualState[W, I, O, Unit] = State { individual =>
    assert(data.size == individual.genome.inputs.size, "Wrong number of inputs")
    individual.genome.inputs.zip(data).map {
      case (i, v) => setNode(i, v)
    }.toList.sequence.run(individual).value._1 -> ()
  }

  def setBias[W, I <: Int, O <: Int](data: Seq[W]): IndividualState[W, I, O, Unit] = State { individual =>
    assert(data.size == individual.genome.bias.size, "Wrong number of bias inputs")
    individual.genome.bias.zip(data).map {
      case (i, v) => setNode(i, v)
    }.toList.sequence.run(individual).value._1 -> ()
  }

  def setNode[W, I <: Int, O <: Int](i: Int, v: W): IndividualState[W, I, O, Unit] = State { individual =>
    individual.copy(nodes = individual.nodes.updated(i, Some(v))) -> ()
  }

  def nodeValue[W, I <: Int, O <: Int](i: Int, visited: Set[Int] = Set.empty)(using F: Fractional[W]): IndividualState[W, I, O, W] = for
    individual <- State.get[Individual[W, I, O]]
    genome = individual.genome
    nodeValue <- individual.nodes(i) match
      case Some(value) => State.pure(value)
      case None if visited.contains(i) =>
        State.pure(F.zero)
      case None =>
        val connections = genome.genes.filter(gene => gene.to == i && gene.enabled)
        assert(!visited.contains(i), "Circular")
        assert(connections.nonEmpty, "Cannot evaluate node without inputs")
        connections.map { gene =>
          nodeValue(gene.from, visited + i).flatMap { n =>
            State.pure(gene.weight * n)
          }
        }.toList.sequence.map(inputs =>
         individual.transfer(inputs.sum)
        )
    _ <- setNode(i, nodeValue)
  yield nodeValue

  def getOutputs[W: Fractional, I <: Int, O <: Int]: IndividualState[W, I, O, Seq[W]] = for
    individual <- State.get[Individual[W, I, O]]
    outputs <- individual.genome.outputs.map(i => nodeValue(i)).toList.sequence
  yield outputs.toSeq
end Individual