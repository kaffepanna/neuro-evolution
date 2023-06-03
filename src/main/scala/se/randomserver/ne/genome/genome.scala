package se.randomserver.ne.genome

import cats.{Applicative, FlatMap, Functor, Monad, MonadError, Order}
import cats.data.StateT
import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.all.{*, given}
import se.randomserver.ne.RandomRange
import se.randomserver.ne.RandomRange.given
import se.randomserver.ne.genome.GenePool.GenePoolStateT

import scala.annotation.unused

case class Gene[W](innovation: Long, from: Int, to: Int, weight: W, enabled: Boolean = true) {
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


case class Genome[W, I <: Int, O <: Int](nInputs: I, nOutputs: O, genes: Set[Gene[W]]):
  val bias:    Range.Exclusive = Range(0, 1)
  val inputs:  Range.Exclusive = Range(bias.end, bias.end + nInputs)
  val outputs: Range.Exclusive = Range(inputs.end, inputs.end + nOutputs)
  val hidden:  Range.Exclusive = Range(
    outputs.end,
    genes.flatMap(g => Set(g.from, g.to)).maxOption.map(_ + 1).getOrElse(outputs.end)
  )
  val nodes:   Range.Exclusive = Range(bias.start, hidden.end)

  def compare(rhs: Genome[W, I, O])(using N: Fractional[W]): Double =
    import N.*
    import Genome.*
    val min_len = List(this.genes.size, rhs.genes.size).min
    val max_len = List(this.genes.size, rhs.genes.size).max

    val d = fromInt(this.genes.diff(rhs.genes).size) / fromInt(max_len)
    val e = fromInt((this.genes.size - rhs.genes.size).abs) / fromInt(max_len)
    val w = this.genes.flatMap { g =>
      rhs.genes.find(g2 => g2.innovation == g.innovation).map(g2 => (g.weight - g2.weight).abs)
    }.sum / fromInt(min_len)

    C1*toDouble(e) + C2*toDouble(d) + C3*toDouble(w)

  override def toString: String = "[" ++ genes.map(_.innovation).mkString(", ") ++ "]"
end Genome

case class GenePool(nextId: Int, innovations: Map[(Int, Int), Long])

object GenePool:
  import scala.deriving.*
  opaque type GenePoolStateT[F[_], A] = StateT[F, GenePool, A]
  given [F[_]](using ap: Applicative[StateT[F, GenePool, _]]): Applicative[GenePoolStateT[F, _]] = ap
  given [F[_]](using ap: Monad[StateT[F, GenePool, _]]): Monad[GenePoolStateT[F, _]] = ap

  def liftF[F[_]: Applicative, A](fa: F[A]): GenePoolStateT[F, A] = StateT.liftF(fa)

  def run[F[_]: FlatMap, A](initial: GenePool)(runner: GenePoolStateT[F, A]): F[(GenePool, A)] = runner.run(initial)

  def nextNodeId[F[_]: Applicative]: GenePoolStateT[F, Int] = StateT(state =>
    (state.copy(nextId = state.nextId + 1) -> state.nextId).pure[F]
  )

  def innovation[F[_]: Applicative](from: Int, to: Int): GenePoolStateT[F, Long] = StateT { state =>
    lazy val nextInnovation = state.innovations.values.maxOption.map(_ + 1).getOrElse(0L)
    val innovation = state.innovations.getOrElse((from, to), nextInnovation)
    val updatedInnovations = state.innovations.updated((from, to), innovation)
    (state.copy(innovations = updatedInnovations) -> innovation).pure[F]
  }

  def newConnection[F[_]: Monad: Random, W](from: Int, to: Int)(using RR: RandomRange[F, W]): GenePoolStateT[F, Gene[W]] = for {
    i      <- innovation(from, to)
    weight <- StateT.liftF(RR.get)
  } yield Gene(i, from, to, weight)

  def genome[F[_]: Monad: Random, W, I <: Int: ValueOf, O <: Int: ValueOf](using RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = for
    initial <- StateT.pure(Genome[W, I, O](valueOf[I], valueOf[O], genes = Set.empty))
    in2out <- initial.inputs.flatMap(i => initial.outputs.map(o => newConnection(i, o))).toList.sequence
    bias2out <- initial.outputs.map(o => newConnection(initial.bias.start, o)).toList.sequence
  yield initial.copy(genes = (in2out ++ bias2out).toSet)

  def cross[F[_]: Monad, W, I <: Int, O <: Int](genome1: Genome[W, I, O], genome2: Genome[W, I, O]): GenePoolStateT[F, Genome[W, I, O]] =
    StateT.pure(Genome(genome1.nInputs, genome1.nOutputs, genome1.genes union genome2.genes))

  @unused
  def mutateAddConnection[F[_]: Monad, W, I <: Int, O <: Int](genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = for
    from <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    to <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    result <- if genome.genes.exists(g => g.from == from && g.to == to) then StateT.pure(genome)
              else if from == to then StateT.pure(genome)
              else newConnection(from, to).map(g => genome.copy(genes = genome.genes + g))
  yield result

  @unused
  def mutateAddNode[F[_]: Monad, W, I <: Int, O <: Int](genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = for
    nextId <- StateT.pure { genome.nodes.end }
    oldConn <- StateT.liftF {
      r.shuffleList(genome.genes.toList.filterNot(_.from == genome.bias.start)).map(_.head)
    }
    c1 <- newConnection(oldConn.from, nextId)
    c2 <- newConnection(nextId, oldConn.to)
    bias <- newConnection(genome.bias.start, nextId)
  yield genome.copy(
    genes = genome.genes - oldConn + oldConn.copy(enabled = false) + c1 + c2 // + bias
  )

  @unused
  def mutateWeight[F[_]: Monad, W: Order: Numeric, I <: Int, O <: Int](genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W], N: Numeric[W]): GenePoolStateT[F, Genome[W, I ,O]] = {
    import N._
    for {
      conn <- StateT.liftF { r.shuffleList(genome.genes.toList.filter(_.enabled)).map(_.head) }
      change <- StateT.liftF(RR.get)
      weightChanged = (fromInt(-2), (fromInt(2), conn.weight + change).maximum).minimum
      updated = conn.copy(weight = conn.weight + weightChanged)
    } yield genome.copy(genes = genome.genes - conn + updated)
  }

  @unused
  def chance[F[_]: Monad, W, I <: Int, O <: Int](c: Double)(eff: Genome[W, I, O] => GenePoolStateT[F, Genome[W, I, O]])(using r: Random[F]): Genome[W, I, O] => GenePoolStateT[F, Genome[W, I, O]] = genome =>
    StateT.liftF(r.nextDouble).flatMap {
      case p if p < c => eff(genome)
      case _          => StateT.pure(genome)
    }

  @unused
  def mutate[F[_]: Monad: Random, W: Order: Numeric, I <: Int, O <: Int](weightChance: Double, connectionChance: Double, nodeChance: Double)(using RandomRange[F, W])(g: Genome[W, I, O]): GenePoolStateT[F, Genome[W, I, O]] =
    chance(weightChance)(mutateWeight[F, W, I, O]).apply(g)
      >>= chance(connectionChance)(mutateAddConnection)
      >>= chance(nodeChance)(mutateAddNode)
end GenePool
