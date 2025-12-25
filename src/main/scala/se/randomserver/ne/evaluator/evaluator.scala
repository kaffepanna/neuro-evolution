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

  final case class ActivationState[W](values: Vector[W])

  extension [W](v: Vector[W])
    def updateAll(map: Map[Int, W]): Vector[W] = map.foldLeft(v) {
      case (v, i) => v.updated(i._1, i._2)
    }

  def topoSort[W, I <: Int, O <: Int](
    genome: Genome[W, I, O]
  ): Vector[Int] = {

    val nodes = genome.nodes.toVector
    val edges = genome.genes.filter(_.enabled).map(g => g.from -> g.to)

    // 1. Initialize indegree for ALL nodes
    val indegree0 =
      nodes.map(_ -> 0).toMap

    val indegree =
      edges.foldLeft(indegree0) {
        case (m, (_, to)) => m.updated(to, m(to) + 1)
      }

    // 2. Adjacency list
    val outgoing =
      edges.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
        .withDefaultValue(Vector.empty)

    // 3. Kahn's algorithm
    @annotation.tailrec
    def loop(
      queue: Vector[Int],
      indeg: Map[Int, Int],
      acc: Vector[Int]
    ): Vector[Int] =
      queue match {
        case Vector() => acc
        case n +: rest =>
          val (newIndeg, newlyZero) =
            outgoing(n).foldLeft((indeg, Vector.empty[Int])) {
              case ((m, zs), to) =>
                val d = m(to) - 1
                val m2 = m.updated(to, d)
                if (d == 0) (m2, zs :+ to) else (m2, zs)
            }

          loop(rest ++ newlyZero, newIndeg, acc :+ n)
      }

    val start = indegree.collect { case (n, 0) => n }.toVector
    val result = loop(start, indegree, Vector.empty)

    assert(
      result.size == nodes.size,
      "Cycle detected or topoSort dropped nodes"
    )

    result
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

    //assert(order.map(_.id).toSet == genome.nodes.toSet)

    CompiledNetwork(
      inputs  = genome.inputs.toVector,
      bias    = genome.bias.toVector,
      outputs = genome.outputs.toVector,
      order   = order,
      transfer = transfer
    )
  }

  def step[W: Fractional](
    net: CompiledNetwork[W],
    prev: ActivationState[W],
    inputs: Map[Int, W],
    biasValue: W
  ): ActivationState[W] = {

    val F = summon[Fractional[W]]

    val biases = net.bias.map(_ -> biasValue).toMap

    val withInputs = prev.copy(
      values = prev.values.updateAll(inputs).updateAll(biases)
    )

    val nextValues =
      net.order.foldLeft(withInputs.values) { (values, node) =>
        if (net.inputs.contains(node.id) || net.bias.contains(node.id))
          values
        else {
          val sum =
            node.incoming.map(c => values(c.from) * c.weight).sum
          values.updated(node.id, net.transfer(sum))
        }
      }

    ActivationState(nextValues)
  }

  def evalOnce[W: Fractional](
    net: CompiledNetwork[W],
    inputs: Map[Int, W],
    biasValue: W
  ): Vector[W] = {

    val zero = summon[Fractional[W]].zero
    val nodeCount = net.order.map(_.id).max + 1
    val init = ActivationState(Vector.fill(nodeCount)(zero))

    val finalState = step(net, init, inputs, biasValue)
    net.outputs.map(finalState.values)
  }
}