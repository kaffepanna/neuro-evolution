package se.randomserver.ne.evolution

import cats.*
import cats.syntax.all.*
import cats.data.StateT
import cats.effect.std.Random
import cats.effect.IO
import se.randomserver.ne.RandomRange
import se.randomserver.ne.genome.GenePool.{GenePoolStateT, cross, mutate, given}
import se.randomserver.ne.genome.{GenePool, Genome}
import se.randomserver.ne.phenotype.Individual
import se.randomserver.ne.phenotype.Individual.*

object Evolution:

  private def classify[W, I <: Int, O <: Int](population: List[Genome[W, I, O]])(using F: Fractional[W]): List[List[Genome[W, I, O]]] =
    population match
      case archetype :: Nil => List(List(archetype))
      case archetype :: rest =>
        val (classified, tobe) = rest.foldLeft(List(archetype) -> List.empty[Genome[W, I, O]]) {
          case ((same, rest), g) => if g.compare(archetype) < 0.5 then (g :: same, rest)
                                                                  else (same, g :: rest)
        }
        classified :: classify(tobe)
      case _ => List()

  private def constructor[W, I <: Int, O <: Int](using r: Random[IO], rr: RandomRange[IO, W], vI: ValueOf[I], vO: ValueOf[O]): GenePoolStateT[IO, Genome[W, I, O]] =
    GenePool.genome[IO, W, I, O]


  def evolve[W: Order: Numeric, I <: Int: ValueOf, O <: Int: ValueOf, S: Ordering](
    data: Set[(List[W], List[W])],
    transfer: W => W,
    fitnessFn: (List[List[W]], List[List[W]]) => S,
    popsize: Int,
    breed: Int,
    gens: Int,
    defaultBias: W,
    minScore: Option[S]
  )(using r: Random[IO], rr: RandomRange[IO, W], f: Fractional[W]): GenePoolStateT[IO, List[(Genome[W, I, O], S)]] =
    for {
      initialPopulation <- constructor[W, I, O].replicateA(popsize)

      endpool <- 1.to(gens).toList.foldM(initialPopulation) { case (pool, _) =>
        // compute scores by evaluating each genome over the full dataset, then calling the provided fitnessFn
        val spiecies = classify(pool).map { pop =>
          pop.map { g =>
            val outputsList: List[List[W]] = data.toList.map { case (inputs, _) => Individual.evaluate(g, transfer, inputs, defaultBias) }
            val expectedList: List[List[W]] = data.toList.map { case (_, expected) => expected }
            val score: S = fitnessFn(outputsList, expectedList)
            g -> score
          }
        }.map(_.sortBy(_._2).reverse)

        val operations = spiecies.flatMap { klass =>

          val breeders = klass.take(breed)
          val breedersPure: Seq[GenePoolStateT[IO, Genome[W, I, O]]] = breeders.map(_._1).map(g => g.pure)

          // Number of genomes we need to produce for this species to keep the same size
          val needed = klass.size - breedersPure.size

          val children: Seq[GenePoolStateT[IO, Genome[W, I, O]]] =
            if (needed <= 0) Seq.empty
            else if (breeders.size >= 2) {
              val pairChildren: Seq[GenePoolStateT[IO, Genome[W, I, O]]] = for {
                (g1, _) <- breeders
                (g2, _) <- breeders
              } yield cross[IO, W, I, O](g1, g2) >>= mutate[IO, W, I, O](0.7, 0.4, 0.01)

              if (pairChildren.isEmpty) Seq.empty
              else Iterator.continually(pairChildren).flatten.take(needed).toSeq
            } else if (breeders.size == 1) {
              Seq.fill(needed)(breeders.head._1.pure)
            } else {
              val clones: Seq[GenePoolStateT[IO, Genome[W, I, O]]] = klass.map { case (g, _) => g.pure }
              if (clones.isEmpty) Seq.empty
              else Iterator.continually(clones).flatten.take(needed).toSeq
            }

          (breedersPure ++ children).take(klass.size)
        }
        operations.sequence
      }

      scored = classify(endpool).map { pop =>
        pop.map { g =>
          val outputsList: List[List[W]] = data.toList.map { case (inputs, _) => Individual.evaluate(g, transfer, inputs, defaultBias) }
          val expectedList: List[List[W]] = data.toList.map { case (_, expected) => expected }
          val score: S = fitnessFn(outputsList, expectedList)
          g -> score
        }
      }.map(list => list.maxBy(_._2)).sortBy(_._2)

      // Return top performer per species that passes the optional minScore
      winners = minScore match
        case Some(ms) => scored.filter { case (_, s) => Ordering[S].gteq(s, ms) }
        case None     => scored
    } yield winners

end Evolution
