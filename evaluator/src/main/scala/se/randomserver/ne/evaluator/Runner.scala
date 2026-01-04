package se.randomserver.ne.evaluator

import se.randomserver.ne.evaluator.Compiler.*
import spire.algebra.{Field, Monoid, Semigroup, Ring}
import spire.syntax.all.*
import cats.syntax.all.*
import algebra.ring.Semiring

object Runner {

  opaque type ActivationState[W] = Vector[W]

  object ActivationState {
    def zero[W: Semiring](n: Int): ActivationState[W] = Vector.fill(n)(Semiring[W].zero)
    def apply[W](s: W*): ActivationState[W] = Vector(s*)
  }

  extension [W](v: ActivationState[W])
    def apply(i: Int): W = v(i)
    def updateAll(map: Map[Int, W]): ActivationState[W] = map.foldLeft(v) {
      case (v, i) => v.updated(i._1, i._2)
    }
    def updated(i: Int, value: W) = v.updated(i, value)

  def stepBlock[W: Semiring](
    block: CompiledBlock[W],
    prev: ActivationState[W], // activation state with inputs set
    network: CompiledNetwork[W]
  ): ActivationState[W] = {
    block.nodes.foldLeft(prev) { (values, node) =>
      if (network.inputs.contains(node.id) || network.bias.contains(node.id))
        values
      else
        val x = node.incoming.map(c => prev(c.from) * c.weight).fold(Semiring[W].zero)(_ + _)
        values.updated(node.id, network.transfer(x))
    }
  }

  def stepNetwork[W: Semiring](
    network: CompiledNetwork[W],
    inputs: Map[Int, W],
    biasValue: W,
    activationState: ActivationState[W]
  ): ActivationState[W] = {
      val stateWithInputs = activationState
        .updateAll(inputs)
        .updateAll(network.bias.map(_ -> biasValue).toMap)

      network.blocks.foldLeft(stateWithInputs) { (state, block) =>
        stepBlock(block, state, network)   
      }
    }

  def evalNetwork[W: Semiring](
    network: CompiledNetwork[W],
    inputs: Map[Int, W],
    biasValue: W,
    recurrentSteps: Int
  ): Vector[W] = {
    val maxNodeId = network.blocks.flatMap(_.nodes.map(_.id)).max
    val init = ActivationState.zero(maxNodeId + 1)
      .updateAll(inputs)
      .updateAll(network.bias.map(_ -> biasValue).toMap)
    
    val finalState = network.blocks.foldLeft(init) { (state, block) =>
      if (block.isRecurrent)
        Iterator.iterate(init)(s => stepBlock(block, s, network)).drop(recurrentSteps).next()
      else
        stepBlock(block, state, network)
    }

    network.outputs.toVector.sorted.map(finalState.apply)
  }
}
