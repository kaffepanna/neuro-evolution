package se.randomserver.ne.genome

import cats.{Applicative, FlatMap, Functor, Monad, MonadError, Order}
import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import spire.syntax.all.*
import spire.math.*
import spire.implicits.given
import spire.compat.ordering
import algebra.ring.Field
import se.randomserver.ne.genome.RandomRange
import se.randomserver.ne.genome.RandomRange.given

import scala.annotation.unused
import pureconfig.ConfigReader
import algebra.ring.Semiring
import algebra.ring.Ring
import algebra.ring.Signed
import cats.mtl.Stateful

final case class Gene[W](innovation: Long, from: Int, to: Int, weight: W, enabled: Boolean = true)

case class SpeciationConfig(
  c1: Double,
  c2: Double,
  c3: Double,
  threshold: Double
) derives ConfigReader

final case class Genome[W](nInputs: Int, nOutputs: Int, genes: Set[Gene[W]]):
  val bias:    Range.Exclusive = Range(0, 1)
  val inputs:  Range.Exclusive = Range(bias.end, bias.end + nInputs)
  val outputs: Range.Exclusive = Range(inputs.end, inputs.end + nOutputs)
  val hidden:  Range.Exclusive = Range(
    outputs.end,
    genes.flatMap(g => Set(g.from, g.to)).maxOption.map(_ + 1).getOrElse(outputs.end)
  )
  val nodes:   Range.Exclusive = Range(bias.start, hidden.end)

  def compare(rhs: Genome[W], cfg: SpeciationConfig)(using Field[W], Signed[W], Order[W]): W =
    import Genome.*
    val Zero = Field[W].zero

    val C1 = Field[W].fromDouble(cfg.c1)
    val C2 = Field[W].fromDouble(cfg.c2)
    val C3 = Field[W].fromDouble(cfg.c3)

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
    val excess = Field[W].fromInt(excessThis + excessRhs)

    // 2. Disjoint genes
    val disjointThis = Field[W].fromInt(
      genesThis.count(g =>
        g.innovation <= maxInnovRhs && !mapRhs.contains(g.innovation)
      )
    )

    val disjointRhs =
      genesRhs.count(g =>
        g.innovation <= maxInnovThis && !mapThis.contains(g.innovation)
      )

    val disjoint = disjointThis + disjointRhs

    // 3. Weight differences of matching genes
    val matchingInnovations = mapThis.keySet.intersect(mapRhs.keySet)
    val weightDiff = if (matchingInnovations.isEmpty) Zero
                    else matchingInnovations.toList.map { innov =>
                      (mapThis(innov).weight - mapRhs(innov).weight).abs
                    }.fold(Zero)(_ + _) / Field[W].fromInt(matchingInnovations.size)

    // 4. Normalize by genome size (optional, NEAT divides by N)
    val N = List(genesThis.size, genesRhs.size).map(Field[W].fromInt).max.max(Field[W].one) // avoid division by 0

    // 5. Weighted sum using NEAT constants
    C1 * (excess / N) + C2 * (disjoint / N) + C3 * weightDiff

  override def toString: String = "[" ++ genes.map(_.innovation).mkString(", ") ++ "]"
end Genome

final case class GenePool(nextId: Int, innovations: Map[(Int, Int), Long])

type HasGenePool[F[_]] = Stateful[F, GenePool]

//trait HasGenePool[F[_]] {
//  def apply[B] = modifyF
//  def modifyF[B](ap: GenePool => F[(GenePool, B)]): F[B]
//  def get: F[GenePool]
//  def set(state: GenePool): F[Unit]
//}

object HasGenePool {
  def apply[F[_]](using gp: HasGenePool[F]): HasGenePool[F] = gp
}

object GenePool:
  def modifyGenePool[F[_]: Monad: HasGenePool, B](fn: GenePool => F[(GenePool, B)]): F[B] = for {
    state <- HasGenePool[F].get
    result <- fn(state)
    (newstate, b) = result
    _ <- HasGenePool[F].set(newstate)
  } yield b

  def nextNodeId[F[_]: Monad: HasGenePool]: F[Int] = modifyGenePool { state =>
    (state.copy(nextId = state.nextId + 1) -> state.nextId).pure
  }

  def innovation[F[_]: Monad: HasGenePool](from: Int, to: Int): F[Long] = modifyGenePool { state =>
    lazy val nextInnovation = state.innovations.values.maxOption.map(_ + 1).getOrElse(0L)
    val innovation = state.innovations.getOrElse((from, to), nextInnovation)
    val updatedInnovations = state.innovations.updated((from, to), innovation)
    (state.copy(innovations = updatedInnovations) -> innovation).pure[F]
  }

  def newConnection[F[_]: Monad: HasGenePool: Random, W](from: Int, to: Int, weight: Option[W] = None)(using RR: RandomRange[F, W]): F[Gene[W]] = for {
    i <- innovation(from, to)
    w <- weight match {
      case Some(w) => w.pure
      case None => RR.get
    }
  } yield Gene(i, from, to, w)

  def genome[F[_]: Monad: Random: HasGenePool, W](nInputs: Int, nOutputs: Int)(using RandomRange[F, W]): F[Genome[W]] = for
    initial <- Genome[W](nInputs, nOutputs, genes = Set.empty).pure
    in2out <- initial.inputs.flatMap(i => initial.outputs.map(o => newConnection(i, o))).toList.sequence
    bias2out <- initial.outputs.map(o => newConnection(initial.bias.start, o)).toList.sequence
  yield initial.copy(genes = (in2out ++ bias2out).toSet)

  def cross_[F[_]: Applicative, W ](genome1: Genome[W], genome2: Genome[W]): F[Genome[W]] =
    Genome(genome1.nInputs, genome1.nOutputs, genome1.genes union genome2.genes).pure

  def cross[F[_]: Monad: Random: HasGenePool, W](fitter: Genome[W], other: Genome[W]): F[Genome[W]] = {
    val mapFit = fitter.genes.map(g => g.innovation -> g).toMap
    val mapOther = other.genes.map(g => g.innovation -> g).toMap
    val allInnovations = mapFit.keySet ++ mapOther.keySet

    {
      allInnovations.toList.traverse { innov =>
        (mapFit.get(innov), mapOther.get(innov)) match {
        // Matching gene
          case (Some(g1), Some(g2)) =>
            for {
              pickFirst <- Random[F].nextBoolean
              enabled   <- if (!g1.enabled || !g2.enabled)
                            Random[F].nextDouble.map(_ > 0.25) // 75% disabled
                          else true.pure
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

  private def validTargets[W](genome: Genome[W], from: Int): Vector[Int] =
    from match {
      case _ if genome.bias.contains(from) =>
        (genome.hidden ++ genome.outputs).toVector

      case _ if genome.inputs.contains(from) =>
        (genome.hidden ++ genome.outputs).toVector

      case _ if genome.hidden.contains(from) =>
        (genome.hidden ++ genome.outputs).toVector

      case _ =>
        Vector.empty
    }
  
  @unused
  private def createsCycle[W, I <: Int, O <: Int](
    genome: Genome[W],
    from: Int,
    to: Int
  ): Boolean = {

    def dfs(current: Int, visited: Set[Int]): Boolean =
      if (current == from) true
      else if (visited.contains(current)) false
      else {
        val next =
          genome.genes.collect {
            case g if g.from == current => g.to
          }
        next.exists(n => dfs(n, visited + current))
      }

    dfs(to, Set.empty)
  }

  def mutateAddConnection[F[_]: Monad: HasGenePool, W](genome: Genome[W])(using r: Random[F], RR: RandomRange[F, W]): F[Genome[W]] = 
    val validFrom = genome.nodes.filterNot(genome.outputs.contains)
    for {
      from <- r.shuffleList(validFrom.toList).map(_.head)
      toCandidates = validTargets(genome, from).filterNot(to =>
            genome.genes.exists(g => g.from == from && g.to == to)
          )
          //.filterNot(to => createsCycle(genome, from, to))
      
      toOpt <- r.shuffleList(toCandidates.toList).map(_.headOption)
      result <- toOpt match {
        case None => genome.pure
        case Some(to) => 
          if (genome.genes.exists(g => g.from == from && g.to == to)) genome.pure
          else newConnection(from, to).map(g => genome.copy(genes = genome.genes + g))
      }
    } yield result

  @unused
  def mutateAddNode[F[_]: Monad: HasGenePool, W](genome: Genome[W])(using r: Random[F], RR: RandomRange[F, W]): F[Genome[W]] = for
    nextId <- genome.nodes.end.pure
    oldConn <-
      r.shuffleList(
        genome.genes
          .filterNot(_.from == genome.bias.start)
          .filter(_.enabled)
          .toList
        ).map(_.head)
  
    c1 <- newConnection(oldConn.from, nextId, Some(oldConn.weight))
    c2 <- newConnection(nextId, oldConn.to, Some(RR.one))
  yield genome.copy(
    genes = genome.genes - oldConn + oldConn.copy(enabled = false) + c1 + c2
  )

  @unused
  def mutateWeight[F[_]: Monad, W: Order: Semiring](resetChance: Double)(genome: Genome[W])(using r: Random[F], RR: RandomRange[F, W]): F[Genome[W]] = {
    for {
      conn <- r.shuffleList(genome.genes.toList.filter(_.enabled)).map(_.head)
      newWeight <- chanceT[F, GenePool, W](resetChance)(
        _ => RR.get,
        w => RR.perturb.map(d => RR.clamp(w + d))
      ).apply(conn.weight)
      updated = conn.copy(weight = newWeight)
    } yield genome.copy(genes = genome.genes - conn + updated)
  }

  @unused
  def chance[F[_]: Monad, A](p: Double)(hit: A => F[A], miss: A => F[A])(using r: Random[F]): A => F[A] = a =>
    r.nextDouble.flatMap { d => if (d < p) hit(a) else miss(a) }

  def chanceT[F[_]: Monad, S, A](p: Double)(
                                            hit: A => F[A],
                                            miss: A => F[A]
                                           )(using r: Random[F]): A => F[A] = a =>
    r.nextDouble.flatMap { d => if (d < p) hit(a) else miss(a) }

  def chanceT[F[_]: Monad, S, A](p: Double)(hit: A => F[A])(using r: Random[F]): A => F[A] =
    chanceT(p)(hit, Monad[F].pure)

  @unused
  def mutate[F[_]: Monad: Random: HasGenePool, W: Order: Semiring](weightChance: Double, connectionChance: Double, nodeChance: Double, resetChance: Double)(using RandomRange[F, W])(g: Genome[W]): F[Genome[W]] =
    chanceT(weightChance)(mutateWeight[F, W](resetChance)).apply(g)
      >>= chanceT(connectionChance)(mutateAddConnection)
      >>= chanceT(nodeChance)(mutateAddNode)
end GenePool
