package se.randomserver.ne.evaluator

import se.randomserver.ne.genome.Genome
import scala.math.Numeric.Implicits.infixNumericOps


object Evaluator {
  final case class CompiledConnection[W](
    from: Int,
    weight: W
  )

  final case class CompiledNode[W](
    id: Int,
    incoming: Vector[CompiledConnection[W]]
  )

  final case class CompiledNetwork[W](
    inputs: Vector[Int],
    bias: Vector[Int],
    outputs: Vector[Int],
    order: Vector[CompiledNode[W]], // topologically sorted
    transfer: W => W
  )

  def topoSort[W, I <: Int, O <: Int](
    genome: Genome[W, I, O]
  ): Vector[Int] = {

    val enabledGenes = genome.genes.filter(_.enabled)

    val incomingCount =
      enabledGenes
        .groupBy(_.to)
        .view
        .mapValues(_.size)
        .toMap
        .withDefaultValue(0)

    val outgoing =
      enabledGenes
        .groupBy(_.from)
        .view
        .mapValues(_.map(_.to))
        .toMap
        .withDefaultValue(Vector.empty)

    val start =
      genome.nodes.filter(n => incomingCount(n) == 0)

    @annotation.tailrec
    def loop(
      queue: Vector[Int],
      inCount: Map[Int, Int],
      acc: Vector[Int]
    ): Vector[Int] =
      queue match {
        case Vector() => acc
        case n +: rest =>
          val (nextQueue, nextCount) =
            outgoing(n).foldLeft(rest -> inCount) {
              case ((q, cnt), m) =>
                val c = cnt(m) - 1
                if (c == 0) (q :+ m, cnt.updated(m, 0))
                else (q, cnt.updated(m, c))
            }

          loop(nextQueue, nextCount, acc :+ n)
      }

    loop(start.toVector, incomingCount, Vector.empty)
  }

  def compile[W: Fractional, I <: Int, O <: Int](
    genome: Genome[W, I, O],
    transfer: W => W
  ): CompiledNetwork[W] = {

    val enabledGenes = genome.genes.filter(_.enabled)

    val incoming =
      enabledGenes
        .groupBy(_.to)
        .view
        .mapValues(_.map(g => CompiledConnection(g.from, g.weight)).toVector)
        .toMap
        .withDefaultValue(Vector.empty)

    val order =
      topoSort(genome).map { id =>
        CompiledNode(id, incoming(id))
      }

    CompiledNetwork(
      inputs  = genome.inputs.toVector,
      bias    = genome.bias.toVector,
      outputs = genome.outputs.toVector,
      order   = order,
      transfer = transfer
    )
  }

  def eval[W: Fractional](
    net: CompiledNetwork[W],
    inputs: Map[Int, W],
    biasValue: W
  ): Map[Int, W] = {

    val F = summon[Fractional[W]]

    net.order.foldLeft(Map.empty[Int, W]) { (values, node) =>
      val v =
        if (net.inputs.contains(node.id))
          inputs(node.id)
        else if (net.bias.contains(node.id))
          biasValue
        else {
          val sum =
            node.incoming.map { c =>
              values(c.from) * c.weight
            }.foldLeft(F.zero)(F.plus)

          net.transfer(sum)
        }

      values.updated(node.id, v)
    }
  }

}