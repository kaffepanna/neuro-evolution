package se.randomserver.ne
import cats.*
import cats.data.{State, StateT}
import cats.effect.std.{QueueSource, Random}
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.{*, given}
import se.randomserver.ne.genome.GenePool.{GenePoolStateT, cross, mutate, given}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import se.randomserver.ne.graphviz.GraphViz.edge_
import se.randomserver.ne.graphviz.{GraphViz, StringCompiler}
import se.randomserver.ne.phenotype.Individual
import se.randomserver.ne.phenotype.Individual.*

import scala.annotation.unused
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


object App extends IOApp {

    private def classify(population: List[Genome[Double, 2, 1]]): List[List[Genome[Double, 2, 1]]] = population match
    case archetype :: Nil => List(List(archetype))
    case archetype :: rest =>
      val (classified, tobe) = rest.foldLeft(List(archetype) -> List.empty[Genome[Double, 2, 1]]) {
        case ((same, rest), g) => if g.compare(archetype) < 0.5 then (g :: same, rest)
                                                                else (same, g :: rest)
      }
      classified :: classify(tobe)
    case _ => List()

  private def sigmoid(x: Double) = 1.0d / (1.0d + Math.exp(-x))

  private def eval(input: List[Double], g: Genome[Double, 2, 1]): List[Double] = {
    val bias = g.bias.map(_ => 1.0d)
    (Individual.from(g, sigmoid) >> {
      Individual.setInputs(input)
        >> Individual.setBias(bias)
        >> Individual.getOutputs
    })._2.toList
  }


  private def score(data: Set[(List[Double], List[Double])])(indivl: Genome[Double, 2, 1]): Double = {
    val error = data.foldLeft(0d) {
      case (err, (input, expected)) =>
        val bias = indivl.bias.map(_ => 1.0d)
        val (_, result) = Individual.from(indivl, sigmoid) >> (Individual.setInputs(input) >> Individual.setBias(bias) >> Individual.getOutputs)
        err + result.zip(expected).map { case (a,b) => Math.pow(a-b, 2) }.sum
    }
    1 - Math.sqrt(error / data.size)
  }

  private def scores(data: Set[(List[Double], List[Double])])(population: List[Genome[Double, 2, 1]]): List[(Genome[Double,2 ,1], Double)] =
    population.map { genome =>
          genome -> score(data)(genome)
    }

  private def constructor(using Random[IO]): GenePoolStateT[IO, Genome[Double, 2, 1]] =
    GenePool.genome[IO, Double, 2, 1]

  def plotGenome(g: Genome[Double, 2, 1]): GraphViz.Grammar[Unit] = {
    import GraphViz._
    digraph {
      for {
        inputs <- subgraph("cluster_inputs") {
          g.inputs.toList.map(nid => node(nid.toString)).sequence
        }
        hidden <- subgraph("cluster_hidden") {
          g.hidden.toList.map(nid => node(nid.toString)).sequence
        }
        outputs <- subgraph("cluster_output") {
          g.outputs.toList.map(nid => node(nid.toString)).sequence
        }
        bias <- subgraph("cluster_bias") {
          g.bias.toList.map(nid => node(nid.toString)).sequence
        }
        edges <- g.genes
          .filter(gene => gene.enabled)
          .toList
          .map { gene =>
            edge_(gene.from.toString, gene.to.toString, "label" -> s"w: ${String.format("%f", gene.weight)}")
          }.sequence
      } yield ()
    }
  }

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

    val state = for {
      initialPopulation <- constructor.replicateA(popsize)

      endpool <- 1.to(1000).toList.foldM(initialPopulation) { case (pool, _) =>
        val spiecies = classify(pool).map(scores(data)).map(_.sortBy(_._2).reverse)

        val operations = spiecies.flatMap { klass =>

          val breeders = klass.take(breed)
          val bred = breeders.flatMap { case (genome, _) =>
            breeders.map { case (genome2, _) =>
              cross[IO, Double, 2, 1](genome, genome2) >>= mutate(0.7, 0.4, 0.01)
            }
          }
          val breedersPure: Seq[GenePoolStateT[IO, Genome[Double, 2, 1]]] = breeders.map(_._1).map(g => g.pure)
          val replicated: Seq[GenePoolStateT[IO, Genome[Double, 2, 1]]] = List.fill((popsize-breed)/bred.size)(bred).flatten //(popsize/bred.size).flatten
          breedersPure ++ replicated
        }
        operations.sequence
      }

      scored = classify(endpool).map(scores(data)).map(_.maxBy(_._2)).sortBy(_._2)
      _ <- scored.map {
        case (genome, d) =>
          GenePool.liftF(IO.println(genome -> d)) >>
          GenePool.liftF {
            val result = data.map {
              case (inputs, _) => inputs -> eval(inputs, genome)
            }.mkString("\n\t")
            IO.println("\t" ++ result)
          }
      }.sequence

      graph  = scored.filter(s => s._2 > 0.9).map(w => plotGenome(w._1).foldMap(StringCompiler.compiler()).run._1)
      _ <- GenePool.liftF(graph.map(IO.println(_)).sequence)
    } yield ()


    GenePool.run(GenePool(0, Map.empty))(state).map(_ => ExitCode.Success)
  }
}
