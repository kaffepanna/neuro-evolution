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
import pureconfig.ConfigReader

case class Gene[W](innovation: Long, from: Int, to: Int, weight: W, enabled: Boolean = true) {
  // override def equals(obj: Any): Boolean = obj match
  //   case Gene(_, from2, to2, _, _) => (from2, to2) == (from, to)
  //   case _ => false

  // override def hashCode(): Int = (from, to).hashCode() + 65300
}

case class SpeciationConfig(
  c1: Double,
  c2: Double,
  c3: Double,
  threshold: Double
) derives ConfigReader

object Genome:
  // val C1 = 4.0 // excess factor
  // val C2 = 3.0 // disjoint factor
  // val C3 = 0.9 // weight difference factor
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

  def compare(rhs: Genome[W, I, O], cfg: SpeciationConfig)(using F: Fractional[W]): Double =
    import F.*
    import Genome.*

    val C1 = cfg.c1
    val C2 = cfg.c2
    val C3 = cfg.c3

    val genesThis = this.genes.toList.sortBy(_.innovation)
    val genesRhs  = rhs.genes.toList.sortBy(_.innovation)

    // Max innovation numbers
    val maxInnovThis = genesThis.lastOption.map(_.innovation).getOrElse(0L)
    val maxInnovRhs  = genesRhs.lastOption.map(_.innovation).getOrElse(0L)

    // Split genes into maps for fast lookup
    val mapThis = genesThis.map(g => g.innovation -> g).toMap
    val mapRhs  = genesRhs.map(g => g.innovation -> g).toMap

    // 1. Excess genes
    val excessThis = genesThis.count(_.innovation > maxInnovRhs)
    val excessRhs  = genesRhs.count(_.innovation > maxInnovThis)
    val excess = excessThis + excessRhs

    // 2. Disjoint genes
    val disjointThis =
    genesThis.count(g =>
      g.innovation <= maxInnovRhs && !mapRhs.contains(g.innovation)
    )

    val disjointRhs =
      genesRhs.count(g =>
        g.innovation <= maxInnovThis && !mapThis.contains(g.innovation)
      )

    val disjoint = disjointThis + disjointRhs

    // 3. Weight differences of matching genes
    val matchingInnovations = mapThis.keySet.intersect(mapRhs.keySet)
    val weightDiff = if (matchingInnovations.isEmpty) F.zero
                    else matchingInnovations.toList.map { innov =>
                      abs(mapThis(innov).weight - mapRhs(innov).weight)
                    }.sum / F.fromInt(matchingInnovations.size)

    // 4. Normalize by genome size (optional, NEAT divides by N)
    val N = List(genesThis.size, genesRhs.size).max.toDouble max 1.0 // avoid division by 0

    // 5. Weighted sum using NEAT constants
    C1 * (excess.toDouble / N) + C2 * (disjoint.toDouble / N) + C3 * weightDiff.toDouble

  override def toString: String = "[" ++ genes.map(_.innovation).mkString(", ") ++ "]"
end Genome

case class GenePool(nextId: Int, innovations: Map[(Int, Int), Long])

object GenePool:
  import scala.deriving.*
  opaque type GenePoolStateT[F[_], A] = StateT[F, GenePool, A]
  given [F[_]](using ap: Applicative[StateT[F, GenePool, _]]): Applicative[GenePoolStateT[F, _]] = ap
  given [F[_]](using ap: Monad[StateT[F, GenePool, _]]): Monad[GenePoolStateT[F, _]] = ap

  def liftF[F[_]: Applicative, A](fa: F[A]): GenePoolStateT[F, A] = StateT.liftF(fa)
  def pure[F[_]: Applicative, A](fa: A): GenePoolStateT[F, A] =StateT.pure(fa)

  def run[F[_]: FlatMap, A](initial: GenePool)(runner: GenePoolStateT[F, A]): F[(GenePool, A)] = runner.run(initial)

  extension [F[_], A](et: GenePoolStateT[F, A])
    def run(gp: GenePool)(using F: Monad[F]) =
      et.run(gp)

  def nextNodeId[F[_]: Applicative]: GenePoolStateT[F, Int] = StateT(state =>
    (state.copy(nextId = state.nextId + 1) -> state.nextId).pure[F]
  )

  def innovation[F[_]: Applicative](from: Int, to: Int): GenePoolStateT[F, Long] = StateT { state =>
    lazy val nextInnovation = state.innovations.values.maxOption.map(_ + 1).getOrElse(0L)
    val innovation = state.innovations.getOrElse((from, to), nextInnovation)
    val updatedInnovations = state.innovations.updated((from, to), innovation)
    (state.copy(innovations = updatedInnovations) -> innovation).pure[F]
  }

  def newConnection[F[_]: Monad: Random, W](from: Int, to: Int, weight: Option[W] = None)(using RR: RandomRange[F, W]): GenePoolStateT[F, Gene[W]] = for {
    i      <- innovation(from, to)
    w <- weight match {
      case Some(w) => StateT.pure(w)
      case None => StateT.liftF(RR.get)
    }
  } yield Gene(i, from, to, w)

  def genome[F[_]: Monad: Random, W, I <: Int: ValueOf, O <: Int: ValueOf](using RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = for
    initial <- StateT.pure(Genome[W, I, O](valueOf[I], valueOf[O], genes = Set.empty))
    in2out <- initial.inputs.flatMap(i => initial.outputs.map(o => newConnection(i, o))).toList.sequence
    bias2out <- initial.outputs.map(o => newConnection(initial.bias.start, o)).toList.sequence
  yield initial.copy(genes = (in2out ++ bias2out).toSet)

  def cross_[F[_]: Monad, W, I <: Int, O <: Int](genome1: Genome[W, I, O], genome2: Genome[W, I, O]): GenePoolStateT[F, Genome[W, I, O]] =
    StateT.pure(Genome(genome1.nInputs, genome1.nOutputs, genome1.genes union genome2.genes))

  def cross[F[_]: Monad: Random, W, I <: Int, O <: Int](fitter: Genome[W, I, O], other: Genome[W, I, O]): GenePoolStateT[F, Genome[W, I, O]] = {
    val mapFit = fitter.genes.map(g => g.innovation -> g).toMap
    val mapOther = other.genes.map(g => g.innovation -> g).toMap
    val allInnovations = mapFit.keySet ++ mapOther.keySet

    StateT.liftF {
      allInnovations.toList.traverse { innov =>
        (mapFit.get(innov), mapOther.get(innov)) match {
        // Matching gene
          case (Some(g1), Some(g2)) =>
            for {
              pickFirst <- Random[F].nextBoolean
              enabled   <- if (!g1.enabled || !g2.enabled)
                            Random[F].nextDouble.map(_ > 0.25) // 75% disabled
                          else Monad[F].pure(true)
            } yield {
              val chosen = if (pickFirst) g1 else g2
              chosen.copy(enabled = enabled)
            }

          // Disjoint or excess → inherit from fitter only
          case (Some(g), None) =>
            Monad[F].pure(g)
          case (None, _) =>
            Monad[F].pure(null) // discard
        }
      }.map(_.filter(_ != null).toSet)
    }.map { genes =>
      Genome(fitter.nInputs, fitter.nOutputs, genes)
    }
}
  

  @unused
  def mutateAddConnection[F[_]: Monad, W, I <: Int, O <: Int](genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = 
    val validFrom = genome.nodes.filterNot(genome.outputs.contains)
    for {
      from <- StateT.liftF(r.shuffleList(validFrom.toList).map(_.head))
      to = from match {
        case _ if genome.inputs.contains(from) => (genome.outputs ++ genome.hidden).toSeq
        case _ if genome.hidden.contains(from) => genome.outputs.toSeq
        case _ if genome.bias.contains(from)   => (genome.inputs ++ genome.hidden ++ genome.outputs).toSeq
        case _ if genome.outputs.contains(from) => Seq.empty
      }
      toOpt <- StateT.liftF { r.shuffleList(to.toList).map(_.headOption) }
      result <- toOpt match {
        case None => StateT.pure(genome)
        case Some(to) => 
          if (genome.genes.exists(g => g.from == from && g.to == to)) StateT.pure(genome)
          else newConnection(from, to).map(g => genome.copy(genes = genome.genes + g))
      }
    } yield result

  @unused
  def mutateAddNode[F[_]: Monad, W, I <: Int, O <: Int](genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W]): GenePoolStateT[F, Genome[W, I, O]] = for
    nextId <- StateT.pure { genome.nodes.end }
    oldConn <- StateT.liftF {
      r.shuffleList(
        genome.genes
          .filterNot(_.from == genome.bias.start)
          .filter(_.enabled)
          .toList
        ).map(_.head)
    }
    c1 <- newConnection(oldConn.from, nextId, Some(oldConn.weight))
    c2 <- newConnection(nextId, oldConn.to, Some(RR.one))
  yield genome.copy(
    genes = genome.genes - oldConn + oldConn.copy(enabled = false) + c1 + c2
  )

  @unused
  def mutateWeight[F[_]: Monad, W: Order: Numeric, I <: Int, O <: Int](resetChance: Double)(genome: Genome[W, I, O])(using r: Random[F], RR: RandomRange[F, W], N: Numeric[W]): GenePoolStateT[F, Genome[W, I ,O]] = {
    import N._
    for {
      conn <- StateT.liftF { r.shuffleList(genome.genes.toList.filter(_.enabled)).map(_.head) }
      newWeight <- chanceT[F, GenePool, W](resetChance)(
        _ => StateT.liftF(RR.get),
        w => StateT.liftF(RR.perturb).map(d => RR.clamp(w + d))
      ).apply(conn.weight)
      updated = conn.copy(weight = newWeight)
    } yield genome.copy(genes = genome.genes - conn + updated)
  }

  @unused
  def chance[F[_]: Monad, A](p: Double)(hit: A => F[A], miss: A => F[A])(using r: Random[F]): A => F[A] = a =>
    r.nextDouble.flatMap { d => if (d < p) hit(a) else miss(a) }

  def chanceT[F[_]: Monad, S, A](p: Double)(
                                            hit: A => StateT[F, S, A],
                                            miss: A => StateT[F, S, A]
                                           )(using r: Random[F]): A => StateT[F, S, A] = a =>
    StateT.liftF(r.nextDouble).flatMap { d => if (d < p) hit(a) else miss(a) }

  def chanceT[F[_]: Monad, S, A](p: Double)(hit: A => StateT[F, S, A])(using r: Random[F]): A => StateT[F, S, A] =
    chanceT(p)(hit, StateT.pure)

  @unused
  def mutate[F[_]: Monad: Random, W: Order: Numeric, I <: Int, O <: Int](weightChance: Double, connectionChance: Double, nodeChance: Double, resetChance: Double)(using RandomRange[F, W])(g: Genome[W, I, O]): GenePoolStateT[F, Genome[W, I, O]] =
    chanceT(weightChance)(mutateWeight[F, W, I, O](resetChance)).apply(g)
      >>= chanceT(connectionChance)(mutateAddConnection)
      >>= chanceT(nodeChance)(mutateAddNode)
end GenePool
