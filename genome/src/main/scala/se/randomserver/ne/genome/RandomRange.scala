package se.randomserver.ne.genome
import pureconfig.*
import pureconfig.generic.derivation.default.*
import cats.effect.std.Random
import cats.Order

trait RandomRange[F[_], W] {
  def get: F[W]
  def perturb: F[W]
  def clamp(w: W): W
  def one: W
  def zero: W
}

case class RandomRangeConfig(
  initRange: (Double, Double),
  perturbRange: (Double, Double),
  clampRange: (Double, Double)
) derives ConfigReader

object RandomRange:

  trait RR[F[_]: Random, W] {
    def random(range: (W, W)): F[W]
  }

  object RR {
    def apply[F[_], W](using r: RR[F, W]): RR[F, W] = r
  }

  given [F[_]](using R: Random[F]): RR[F, Double] = new RR {
    def random(range: (Double, Double)): F[Double] = 
      R.betweenDouble(range._1, range._2)
  }

  def apply[F[_]: Random](cfg: RandomRangeConfig): RandomRange[F, Double] =
    apply(cfg.initRange, cfg.perturbRange, cfg.clampRange) 

  def apply[F[_]: Random, A: RR[F, _]: Numeric: Order](
      initRange: (A, A),
      perturbRange: (A, A),
      clampRange: (A, A) 
    ): RandomRange[F, A] = new RandomRange {
      import cats.syntax.order.catsSyntaxOrder
      override def get: F[A] = RR[F, A].random(initRange)
      override def perturb: F[A] = RR[F, A].random(perturbRange)
      override def clamp(w: A): A = w.max(clampRange._1).min(clampRange._2)
      override def one: A = Numeric[A].one
      override def zero: A = Numeric[A].zero
    }
