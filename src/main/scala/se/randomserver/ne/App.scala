package se.randomserver.ne
import cats.*
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.GenePoolStateT


object App extends IOApp {

  def test(using Random[IO]): GenePoolStateT[IO, List[Genome]] = for {
     a <- StateT.pure(1)
    //c <- GenePool.cross(a, b)
    //  .flatMap(GenePool.mutateAddNode(_))
    //  .flatMap(GenePool.mutateAddNode(_))
    //  .flatMap(GenePool.mutateAddConnection(_))
    constructor = GenePool.genome[IO](2, 1)
      .flatMap(GenePool.mutate[IO](0.1d, 0.5d, 0.5d))
    population <- Applicative[GenePoolStateT[IO, _]].replicateA(10, constructor)
    _ <- StateT.liftF { IO.println(population.map(_.toString)) }
  } yield population

  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { random =>
    given Random[IO] = random
    for {
      _ <- IO { println("Hej") }
      a <- test.run(GenePool(0, Map.empty))
    } yield ExitCode.Success
  }
}
