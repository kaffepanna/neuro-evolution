package se.randomserver.ne.genome

import cats.{Applicative, FlatMap, Functor, Monad, MonadError}
import cats.data.StateT
import cats.effect.std.Random
import cats.syntax.all.{*, given}
import se.randomserver.ne.genome.GenePool.GenePoolStateT

import scala.annotation.unused

case class Gene[W, N](innovation: Long, from: N, to: N, weight: W, enabled: Boolean = true) {
  override def equals(obj: Any): Boolean = obj match
    case Gene(_, from2, to2, _, _) => (from2, to2) == (from, to)
    case _ => false

  override def hashCode(): Int = (from, to).hashCode() + 65300
}

object Genome:
  val C1 = 1.0
  val C2 = 1.0
  val C3 = 0.2
end Genome


case class Genome(nInputs: Int, nOutputs: Int, genes: Set[Gene[Double, Int]]):
  val bias:    Range.Exclusive = Range(0, 1)
  val inputs:  Range.Exclusive = Range(bias.end, bias.end + nInputs)
  val outputs: Range.Exclusive = Range(inputs.end, inputs.end + nOutputs)
  val hidden:  Range.Exclusive = Range(
    outputs.end,
    genes.flatMap(g => Set(g.from, g.to)).maxOption.map(_ + 1).getOrElse(outputs.end)
  )
  val nodes:   Range.Exclusive = Range(bias.start, hidden.end)

  def compare(rhs: Genome): Double =
    import Genome.*
    val min_len = List(this.genes.size, rhs.genes.size).min
    val max_len = List(this.genes.size, rhs.genes.size).max

    val d = this.genes.diff(rhs.genes).size / max_len.toDouble
    val e = (this.genes.size - rhs.genes.size).abs / max_len.toDouble
    val w = this.genes.flatMap { g =>
      rhs.genes.find(g2 => g2.innovation == g.innovation).map(g2 => (g.weight - g2.weight).abs)
    }.sum / min_len.toDouble

    C1*e + C2*d + C3*w

  override def toString: String = "[" ++ genes.map(_.innovation).mkString(", ") ++ "]"
end Genome

case class GenePool(nextId: Int, innovations: Map[(Int, Int), Long])

object GenePool:
  type GenePoolStateT[F[_], A] = StateT[F, GenePool, A]
  def nextNodeId[F[_]: Applicative]: GenePoolStateT[F, Int] = StateT(state =>
    (state.copy(nextId = state.nextId + 1) -> state.nextId).pure[F]
  )

  def innovation[F[_]: Applicative](from: Int, to: Int): GenePoolStateT[F, Long] = StateT { state =>
    lazy val nextInnovation = state.innovations.values.maxOption.map(_ + 1).getOrElse(0L)
    val innovation = state.innovations.getOrElse((from, to), nextInnovation)
    val updatedInnovations = state.innovations.updated((from, to), innovation)
    (state.copy(innovations = updatedInnovations) -> innovation).pure[F]
  }

  def newConnection[F[_]: Monad: Random](from: Int, to: Int): GenePoolStateT[F, Gene[Double, Int]] = for {
    i      <- innovation(from, to)
    weight <- StateT.liftF(Random[F].betweenDouble(-1.0d, 1.0d))
  } yield Gene(i, from, to, weight)

  def genome[F[_]: Monad: Random](nInputs: Int, nOutputs: Int): GenePoolStateT[F, Genome] = for
    initial <- StateT.pure(Genome(nInputs, nOutputs, genes = Set.empty))
    in2out <- initial.inputs.flatMap(i => initial.outputs.map(o => newConnection(i, o))).toList.sequence
    bias2out <- initial.outputs.map(o => newConnection(initial.bias.start, o)).toList.sequence
  yield initial.copy(genes = (in2out ++ bias2out).toSet)

  def cross[F[_]: Monad](genome1: Genome, genome2: Genome): GenePoolStateT[F, Genome] = for {
    nInputs  <- StateT.pure { List(genome1.nInputs, genome2.nInputs).max }
    nOutputs <- StateT.pure { List(genome1.nOutputs, genome2.nOutputs).max }
    genes = genome1.genes union genome2.genes
  } yield Genome(nInputs, nOutputs, genes)

  @unused
  def mutateAddConnection[F[_]: Monad](genome: Genome)(using r: Random[F]): GenePoolStateT[F, Genome] = for
    from <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    to <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    result <- if genome.genes.exists(g => g.from == from && g.to == to) then StateT.pure(genome)
              else if from == to then StateT.pure(genome)
              else newConnection(from, to).map(g => genome.copy(genes = genome.genes + g))
  yield result

  @unused
  def mutateAddNode[F[_]: Monad](genome: Genome)(using r: Random[F]): GenePoolStateT[F, Genome] = for
    nextId <- StateT.pure { genome.nodes.end }
    oldConn <- StateT.liftF {
      r.shuffleList(genome.genes.toList.filterNot(_.from == genome.bias.start)).map(_.head)
    }
    c1 <- newConnection(oldConn.from, nextId)
    c2 <- newConnection(nextId, oldConn.to)
    bias <- newConnection(genome.bias.start, nextId)
  yield genome.copy(
    genes = genome.genes - oldConn + oldConn.copy(enabled = false) + c1 + c2 + bias
  )

  @unused
  def mutateWeight[F[_]: Monad](genome: Genome)(using r: Random[F]): GenePoolStateT[F, Genome] = for {
    conn <- StateT.liftF { r.shuffleList(genome.genes.toList.filter(_.enabled)).map(_.head) }
    change <- StateT.liftF(r.betweenDouble(-0.5d, 0.5d))
    updated = conn.copy(weight = conn.weight + change)
  } yield genome.copy(genes = genome.genes - conn + updated)

  @unused
  def chance[F[_]: Monad](c: Double)(eff: Genome => GenePoolStateT[F, Genome])(using r: Random[F]): Genome => GenePoolStateT[F, Genome] = genome =>
    StateT.liftF(r.nextDouble).flatMap {
      case p if p < c => eff(genome)
      case _          => StateT.pure(genome)
    }

  @unused
  def mutate[F[_]: Monad: Random](weightChance: Double, connectionChance: Double, nodeChance: Double)(g: Genome): GenePoolStateT[F, Genome] =
    chance(weightChance)(mutateWeight).apply(g) >>= chance(connectionChance)(mutateAddConnection) >>= chance(nodeChance)(mutateAddNode)
end GenePool
