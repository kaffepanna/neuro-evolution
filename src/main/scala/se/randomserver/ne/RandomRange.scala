package se.randomserver.ne

import cats.effect.std.Random

trait RandomRange[F[_], W] {
  def get: F[W]
}

object RandomRange:
  given [F[_]](using R: Random[F]): RandomRange[F, Double] = new RandomRange[F, Double]:
    override def get: F[Double] = R.betweenDouble(-1.0d, 1.0d)

  given [F[_]](using R: Random[F]): RandomRange[F, Float] = new RandomRange[F, Float]:
    override def get: F[Float] = R.betweenFloat(-1.0f, 1.0f)
