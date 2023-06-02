package se.randomserver.ne
import cats.*
import cats.data.{State, StateT}
import cats.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.{GenePoolStateT, cross, mutate}
import se.randomserver.ne.phenotype.Individual
import se.randomserver.ne.phenotype.Individual.*

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration


object App extends IOApp {

  def classify(population: List[Genome]): List[List[Genome]] = population match
    case archetype :: Nil => List(List(archetype))
    case archetype :: rest =>
      val (classified, tobe) = rest.foldLeft(List(archetype) -> List.empty[Genome]) {
        case ((same, rest), g) => if g.compare(archetype) < 0.4 then (g :: same, rest)
                                                                else (same, g :: rest)
      }
      classified :: classify(tobe)
    case _ => List()

  def score(data: Set[(List[Double], List[Double])])(indivl: Genome): Double = {
    val error = data.foldLeft(0d) {
      case (err, (input, expected)) =>
        val bias = indivl.bias.map(_ => 1.0d)
        val (_, result) = (Individual.setInputs(input) >> Individual.setBias(bias) >> Individual.getOutputs).run(Individual.from(indivl)).value
        err + result.zip(expected).map { case (a,b) => Math.pow(a-b, 2) }.sum
    }
    1 - Math.sqrt(error / data.size)
  }

  def scores(data: Set[(List[Double], List[Double])])(population: List[Genome]): List[(Genome, Double)] =
    population.map { genome =>
          genome -> score(data)(genome)
    }

  def constructor(using Random[IO]): GenePoolStateT[IO, Genome] =
    GenePool.genome[IO](2, 1)

  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { random =>
    given Random[IO] = random
    val xor = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(1.0),
      List(0.0, 1.0) -> List(1.0),
      List(1.0, 1.0) -> List(0.0)
    )

    val and = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(0.0),
      List(0.0, 1.0) -> List(0.0),
      List(1.0, 1.0) -> List(1.0)
    )

    val or = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(1.0),
      List(0.0, 1.0) -> List(1.0),
      List(1.0, 1.0) -> List(1.0)
    )

    val data = and

    val popsize = 10
    val breed = 2

    val state = for {
      initialPopulation <- constructor.replicateA(popsize)

      endpool <- 1.to(100).toList.foldM(initialPopulation) { case (pool, _) =>
        val spiecies = classify(pool).map(scores(data)).map(_.sortBy(_._2).reverse)

        val operations = spiecies.flatMap { klass =>

          val breeders = klass.take(breed)
          val bred = breeders.flatMap { case (genome, _) =>
            breeders.map { case (genome2, _) =>
              cross[IO](genome, genome2) >>= mutate(0.7, 0.01, 0.07)
            }
          }
          val breedersPure: Seq[GenePoolStateT[IO, Genome]] = breeders.map(_._1).map(g => StateT.pure(g))
          val replicated: Seq[GenePoolStateT[IO, Genome]] = List.fill((popsize-breed)/bred.size)(bred).flatten //(popsize/bred.size).flatten
          breedersPure ++ replicated
        }
        operations.sequence
      }

      scored = classify(endpool).map(scores(data)).map(_.maxBy(_._2)).sortBy(_._2)
      _ <- scored.map {
        case (genome, d) => StateT.liftF(IO.println(genome -> d))
      }.sequence
    } yield ()

    state.run(GenePool(0, Map.empty)).map(_ => ExitCode.Success)
  }
}
