package se.randomserver.ne
import cats.*
import cats.data.{State, StateT}
import cats.effect.std.{QueueSource, Random}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.{*, given}
import se.randomserver.ne.genome.GenePool.{GenePoolStateT, given}
import se.randomserver.ne.genome.{GenePool, Genome}
import se.randomserver.ne.evolution.Evolution
import se.randomserver.ne.phenotype.Individual
import se.randomserver.ne.phenotype.Individual.*
import se.randomserver.ne.graphviz.StringCompiler
import se.randomserver.ne.GraphvizHelper

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object App extends IOApp {

  // Evolution logic is moved to `se.randomserver.ne.evolution.Evolution`.

  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { random =>
    given Random[IO] = random

    @unused
    val xor = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(1.0),
      List(0.0, 1.0) -> List(1.0),
      List(1.0, 1.0) -> List(0.0)
    )

    @unused
    val and = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(0.0),
      List(0.0, 1.0) -> List(0.0),
      List(1.0, 1.0) -> List(1.0)
    )

    @unused
    val or = Set(
      List(0.0, 0.0) -> List(0.0),
      List(1.0, 0.0) -> List(1.0),
      List(0.0, 1.0) -> List(1.0),
      List(1.0, 1.0) -> List(1.0)
    )

    val data = xor

    val popsize = 10
    val breed = 2
    val gens = 1000
    val defaultBias = 1.0d
    val threshold = 0.9

    // transfer function for Double genomes
    def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))

    // fitness function using Individual.evaluate (returns genome fitness in [0..1])
    // fitness function that computes a single Double score from all outputs/expected pairs
    val fitnessFn: (List[List[Double]], List[List[Double]]) => Double = (outputsList, expectedList) =>
      val totalSumsq = outputsList.zip(expectedList).map { case (outs, exp) => outs.zip(exp).map { case (a,b) => Math.pow(a-b, 2) }.sum }.sum
      1 - Math.sqrt(totalSumsq / outputsList.size)

    // Run evolution and get top performers per species above minScore (Some(0.9) here)
    GenePool.run(GenePool(0, Map.empty))(Evolution.evolve[Double, 2, 1, Double](data, transferFn, fitnessFn, popsize, breed, gens, defaultBias, Some(0.9))).flatMap {
      case (_, winners) =>

        val graphIO = winners.map { case (genome, _) =>
          GraphvizHelper.plotGenome[IO](genome)
        }.sequence

        graphIO.as(ExitCode.Success)
    }
  }
}
