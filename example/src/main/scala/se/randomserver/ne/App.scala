package se.randomserver.ne
import cats.*
import cats.data.{State, StateT}
import cats.effect.std.{QueueSource, Random}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.{*, given}
import se.randomserver.ne.genome.{GenePool, Genome, HasGenePool}
import se.randomserver.ne.graphviz.StringCompiler
import se.randomserver.ne.graphviz.GraphvizHelper

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import se.randomserver.ne.evolution.EvolutionConfig
import se.randomserver.ne.evolution.Evolution.{HasEvolutionEnv, HasEvolutionState}
import se.randomserver.ne.genome.SpeciationConfig
import pureconfig.ConfigReader
import pureconfig.ConfigSource
import pureconfig.module.catseffect.syntax.{*, given}

import se.randomserver.ne.graphviz.GraphViz
import se.randomserver.ne.evolution.Evolution.EvolutionEnv
import se.randomserver.ne.evolution.Evolution.{*, given}
import se.randomserver.ne.genome.RandomRange
import se.randomserver.ne.genome.RandomRangeConfig
import cats.mtl.Ask
import cats.effect.kernel.Ref
import cats.mtl.Stateful
import spire.implicits.*

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

      evolutionEnv = EvolutionEnv[Double, Double](
        data.toList,
        transferFn,
        fitnessFn,
        popsize = evCfg.populationSize,
        generations = evCfg.generations,
        defaultBias = evCfg.defaultBias,
        weightChance = evCfg.weightChance,
        resetChance = evCfg.resetChance,
        connectionChance = evCfg.connectionChance,
        nodeChance = evCfg.nodeChance,
        eliteFraction = evCfg.eliteFraction,
        minScore = evCfg.targetFitness,
        recurrentSteps = evCfg.recurrentSteps,
        speciationConfig = sCfg,
      )

      evolutionStateRef <- Ref.of[IO, EvolutionState[Double, Double]](EvolutionState[Double, Double]()) //EvolutionState[Double, Double]()
      genePoolStateRef <- Ref.of[IO, GenePool](GenePool(0, Map.empty))

      given HasEvolutionEnv[IO, Double, Double] = Ask.const[IO, EvolutionEnv[Double, Double]](evolutionEnv)
      given HasEvolutionState[IO, Double, Double] = new Stateful[IO, EvolutionState[Double, Double]] {
        override def monad: Monad[IO] = summon[Monad[IO]]
        override def get: IO[EvolutionState[Double, Double]] = evolutionStateRef.get
        override def set(s: EvolutionState[Double, Double]): IO[Unit] = evolutionStateRef.set(s)
      }
      given HasGenePool[IO] = new Stateful[IO, GenePool] {
        override def monad: Monad[IO] = summon[Monad[IO]]
        override def get: IO[GenePool] = genePoolStateRef.get
        override def set(s: GenePool): IO[Unit] = genePoolStateRef.set(s)
      }


      _ <- evolve[IO, Double, 2, 1, Double]
      finalState <- evolutionStateRef.get
      winners = finalState.species.map { species =>
        species.members.maxByOption(finalState.fitness)
      }.flatten.filter(m => finalState.fitness.get(m).exists(_ > 0.85)).sortBy(finalState.population(_).genes.size).map(a => se.randomserver.ne.evaluator.Compiler.compileGenome(finalState.population(a), identity))
      _ <- (for(winner <- winners) yield GraphvizHelper.plotCompiled[IO](winner)).sequence
    } yield (ExitCode.Success)
  }
}
