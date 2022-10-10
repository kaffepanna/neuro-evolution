package se.randomserver.ne

import cats.Monad
import cats.effect.Sync
import cats.effect.std.Random
import cats.Applicative
import cats.syntax.all.{*, given}

object Probability {
  def chance[F[_]: Monad: Random, G[_], B](d: Double)(happened: G[B])(not: G[B]): F[G[B]] =
    summon[Random[F]].nextDouble.flatMap {
      case r if r < d => happened.pure[F]
      case _ => not.pure[F]
    }

}
