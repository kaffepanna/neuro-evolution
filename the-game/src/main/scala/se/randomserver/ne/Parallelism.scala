trait Paralellism[F[_]]:
  val threadCount: Int
  
object Paralellism:
  def apply[F[_]](using p: Paralellism[F]): Paralellism[F] = p
