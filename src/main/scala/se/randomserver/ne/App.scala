package se.randomserver.ne
import cats.*
import cats.data.{State, StateT}
import cats.syntax.all.{*, given}
import cats.effect.{ExitCode, IO, IOApp}
import se.randomserver.ne.genome.{Gene, GenePool, Genome}
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.GenePoolStateT

case class Individual(genome: Genome, nodes: Seq[Option[Double]])

object Individual:
  def from(genome: Genome) = Individual(genome, genome.nodes.map(_ => None))
end Individual

type IndividualState[A] = State[Individual, A]

def setInputs(data: Seq[Double]): IndividualState[Unit] = State { individual =>
  assert(data.size == individual.genome.inputs.size, "Wrong number of inputs")
  individual.genome.inputs.zip(data).map {
    case (i, v) => setNode(i, v)
  }.toList.sequence.run(individual).value._1 -> ()
}

def setBias(data: Seq[Double]): IndividualState[Unit] = State { individual =>
  assert(data.size == individual.genome.bias.size, "Wrong number of bias inputs")
  individual.genome.bias.zip(data).map {
    case (i, v) => setNode(i, v)
  }.toList.sequence.run(individual).value._1 -> ()
}

def setNode(i: Int, v: Double): IndividualState[Unit] = State { individual =>
  individual.copy(nodes = individual.nodes.updated(i, Some(v))) -> ()
}

def nodeValue(i: Int): IndividualState[Double] = for
  individual <- State.get[Individual]
  genome = individual.genome
  nodeValue <- individual.nodes(i) match
    case Some(value) => State.pure(value)
    case None =>
      val connections = genome.genes.filter(gene => gene.to == i)
      assert(connections.nonEmpty, "Cannot evaluate node without inputs")
      connections.map { gene =>
        nodeValue(gene.from).flatMap { n =>
          State.pure(gene.weight * n)
        }
      }.toList.sequence.map(inputs =>
        Math.tanh(inputs.sum)
      )
  _ <- setNode(i, nodeValue)
yield nodeValue

def getOutputs: IndividualState[Seq[Double]] = for
  individual <- State.get[Individual]
  outputs <- individual.genome.outputs.map(i => nodeValue(i)).toList.sequence
yield outputs.toSeq

object App extends IOApp {

  def constructor(using Random[IO]): GenePoolStateT[IO, Genome] =
    GenePool.genome[IO](1, 1)
      >>= { genome =>
        GenePool.newConnection(2,1).flatMap { connection =>
          StateT.pure(genome.copy(genes = Set(connection) ++ genome.genes))
        }
      }
  def test(using Random[IO]): GenePoolStateT[IO, List[Genome]] = for {
    population <- Applicative[GenePoolStateT[IO, _]].replicateA(10, constructor)
    _ <- StateT.liftF { IO.println(population.map(_.toString)) }
    _ <- StateT.liftF{
      val first = population.head
      IO.println(first.inputs) >>
      IO.println(first.hidden) >>
      IO.println(first.outputs)
    }
  } yield population

  override def run(args: List[String]): IO[ExitCode] = Random.scalaUtilRandom[IO].flatMap { random =>
    given Random[IO] = random
    for {
      genepool <- constructor.run(GenePool(0, Map.empty))
      in = Individual.from(genepool._2)
      out = (setBias(Seq(1.0d)) >> setInputs(Seq(0.5d)) >> getOutputs).run(in).value
      _ <- IO.println(out._2)
      _ <- IO.println(out._1)
    } yield ExitCode.Success
  }
}
