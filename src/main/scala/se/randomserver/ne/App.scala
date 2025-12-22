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
import se.randomserver.ne.evolution.EvolutionConfig
import se.randomserver.ne.genome.SpeciationConfig
import pureconfig.ConfigReader
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.{*, given}

import se.randomserver.ne.graphviz.GraphViz

case class AppConfig(
  evolution: EvolutionConfig,
  ranges: RandomRangeConfig,
  speciation: SpeciationConfig,
) derives ConfigReader

object App extends IOApp {

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

  // Evolution logic is moved to `se.randomserver.ne.evolution.Evolution`.

  override def run(args: List[String]): IO[ExitCode] = {

    val data = xor

    // transfer function for Double genomes
    def transferFn(x: Double) = 1.0d / (1.0d + Math.exp(-x))

    // fitness function using Individual.evaluate (returns genome fitness in [0..1])
    // fitness function that computes a single Double score from all outputs/expected pairs
    val fitnessFn: (List[List[Double]], List[List[Double]]) => Double = (outputsList, expectedList) =>
      val totalSumsq = outputsList.zip(expectedList).map { case (outs, exp) => outs.zip(exp).map { case (a,b) => Math.pow(a-b, 2) }.sum }.sum
      1 - Math.sqrt(totalSumsq / outputsList.size)

    for {
      appConfig <- ConfigSource.defaultApplication.loadF[IO, AppConfig]()

      AppConfig(evCfg, rCfg, sCfg) = appConfig

      rnd <- Random.scalaUtilRandom[IO]
      given Random[IO] = rnd
      given RandomRange[IO, Double] = RandomRange(rCfg)

      initialPool = GenePool(0, Map.empty)
      winners <- GenePool.run(initialPool) {
        Evolution.evolve[Double, 2, 1, Double](
          data,
          transferFn,
          fitnessFn,
          evCfg.populationSize,
          evCfg.generations,
          evCfg.defaultBias,
          evCfg.weightChance,
          evCfg.resetChance,
          evCfg.connectionChance,
          evCfg.nodeChance,
          evCfg.eliteFraction,
          evCfg.targetFitness,
          sCfg
        )
      }.map(_._2)

      a <- (for(winner <- winners) yield GraphvizHelper.plotGenome[IO](winner._1)).sequence
    } yield (ExitCode.Success)
  }
}
