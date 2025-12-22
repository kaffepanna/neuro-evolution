package se.randomserver.ne
import pureconfig.*
import pureconfig.generic.derivation.default.*
import cats.effect.std.Random

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
  given [F[_]](using R: Random[F]): RandomRange[F, Double] = new RandomRange[F, Double]:
    override def get: F[Double] = R.betweenDouble(-1.0d, 1.0d)
    override def perturb: F[Double] = R.betweenDouble(-0.2d, 0.2d)
    override def clamp(w: Double): Double = w.max(-8.0).min(8.0)
    override def one: Double = 1.0d
    override def zero: Double = 0.0d

  given [F[_]](using R: Random[F]): RandomRange[F, Float] = new RandomRange[F, Float]:
    override def get: F[Float] = R.betweenFloat(-1.0f, 1.0f)
    override def perturb: F[Float] =  R.betweenFloat(-0.5f, 0.5f)
    override def clamp(w: Float): Float = w.max(-5.0f).min(5.0f)
    override def one: Float = 1.0f
    override def zero: Float = 0.0f

  def apply[F[_]: Random](cfg: RandomRangeConfig): RandomRange[F, Double] = new RandomRange {
    val rr = summon[Random[F]]
    override def get: F[Double] = rr.betweenDouble(cfg.initRange._1, cfg.initRange._2)
    override def perturb: F[Double] = rr.betweenDouble(cfg.perturbRange._1, cfg.perturbRange._2)
    override def clamp(w: Double): Double = w.max(cfg.clampRange._1).min(cfg.clampRange._2)
    override def one: Double = 1.0d
    override def zero: Double = 0.0d
  }
