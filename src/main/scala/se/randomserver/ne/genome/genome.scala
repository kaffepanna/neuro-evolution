package se.randomserver.ne.genome

import cats.{Applicative, FlatMap, Functor, Monad, MonadError}
import cats.data.StateT
import cats.effect.std.Random
import cats.syntax.all.{*, given}
import se.randomserver.ne.Probability
import se.randomserver.ne.genome.GenePool.GenePoolStateT

case class Gene[W, N](innovation: Long, from: N, to: N, weight: W, enabled: Boolean = true) {
  override def equals(obj: Any): Boolean = obj match
    case Gene(_, from2, to2, _, _) => (from2, to2) == (from, to)
    case _ => false

  override def hashCode(): Int = (from, to).hashCode() + 65300
}
case class Genome(nInputs: Int, nOutputs: Int, genes: Set[Gene[Double, Int]]):
  val bias = Range(0, 1)
  val inputs = Range(bias.end, bias.end + nInputs)
  val outputs = Range(inputs.end, inputs.end + nOutputs)
  val hidden =
    val lastNode = genes.flatMap(g => Set(g.from, g.to)).maxOption.map(_ + 1).getOrElse(outputs.end)
    Range(outputs.end, lastNode)
  val nodes = Range(bias.start, hidden.end) //hidden.end)
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

  def mutateAddConnection[F[_]: Monad](genome: Genome)(using r: Random[F]): GenePoolStateT[F, Genome] = for
    from <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    to <- StateT.liftF { r.betweenInt(genome.nodes.start, genome.nodes.end) }
    result <- if genome.genes.exists(g => g.from == from && g.to == to) then StateT.pure(genome)
              else newConnection(from, to).map(g => genome.copy(genes = genome.genes + g))
  yield result

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

  def mutateWeight[F[_]: Monad](genome: Genome)(using r: Random[F]): GenePoolStateT[F, Genome] = for {
    conn <- StateT.liftF { r.shuffleList(genome.genes.toList.filter(_.enabled)).map(_.head) }
    change <- StateT.liftF(r.betweenDouble(-0.5d, 0.5d))
    updated = conn.copy(weight = conn.weight + change)
  } yield genome.copy(genes = genome.genes - conn + updated)

  def chance[F[_]: Monad](c: Double)(eff: Genome => GenePoolStateT[F, Genome])(using r: Random[F]): Genome => GenePoolStateT[F, Genome] = genome =>
    StateT.liftF(r.nextDouble).flatMap {
      case p if p < c => eff(genome)
      case _          => StateT.pure(genome)
    }

  def mutate[F[_]: Monad: Random](weightChance: Double, connectionChance: Double, nodeChance: Double)(g: Genome): GenePoolStateT[F, Genome] =
    chance(weightChance)(mutateWeight).apply(g) >>= chance(connectionChance)(mutateAddConnection) >>= chance(nodeChance)(mutateAddNode)
end GenePool
