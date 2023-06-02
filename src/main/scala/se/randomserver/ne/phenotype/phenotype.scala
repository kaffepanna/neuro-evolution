package se.randomserver.ne.phenotype

import cats.*
import cats.data.{State, StateT}
import cats.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.GenePoolStateT

import scala.runtime.stdLibPatches.Predef.assert

case class Individual(genome: Genome, nodes: Seq[Option[Double]])

object Individual:
  type IndividualState[A] = State[Individual, A]
  def from(genome: Genome) = Individual(genome, genome.nodes.map(_ => None))

  def setInputs(data: Seq[Double]): IndividualState[Unit] = State { individual =>
    assert(data.size == individual.genome.inputs.size, "Wrong number of inputs")
    individual.genome.inputs.zip(data).map {
      case (i, v) => setNode(i, v)
    }.toList.sequence.run(individual).value._1 -> ()
  }

  def setBias(data: Seq[Double]): IndividualState[Unit] = State { individual =>
    assert(data.size == individual.genome.bias.size, "Wrong number of bias inputs")
    individual.genome.bias.zip(data).map {
      case (i, v) => setNode(i, v)
    }.toList.sequence.run(individual).value._1 -> ()
  }

  def setNode(i: Int, v: Double): IndividualState[Unit] = State { individual =>
    individual.copy(nodes = individual.nodes.updated(i, Some(v))) -> ()
  }

  def nodeValue(i: Int, visited: Set[Int] = Set.empty): IndividualState[Double] = for
    individual <- State.get[Individual]
    genome = individual.genome
    nodeValue <- individual.nodes(i) match
      case Some(value) => State.pure(value)
      case None if visited.contains(i) =>
        State.pure(0.0d)
      case None =>
        val connections = genome.genes.filter(gene => gene.to == i && gene.enabled)
        assert(!visited.contains(i), "Circular")
        assert(connections.nonEmpty, "Cannot evaluate node without inputs")
        connections.map { gene =>
          nodeValue(gene.from, visited + i).flatMap { n =>
            State.pure(gene.weight * n)
          }
        }.toList.sequence.map(inputs =>
          //Math.tanh(inputs.sum)
          1.0 / (1.0 + Math.exp(-inputs.sum))
        )
    _ <- setNode(i, nodeValue)
  yield nodeValue

  def getOutputs: IndividualState[Seq[Double]] = for
    individual <- State.get[Individual]
    outputs <- individual.genome.outputs.map(i => nodeValue(i)).toList.sequence
  yield outputs.toSeq
end Individual