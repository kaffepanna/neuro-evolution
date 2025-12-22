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

import scala.math.Fractional.Implicits.infixFractionalOps
import se.randomserver.ne.genome.SpeciationConfig

object Evolution:

  private def classify[W, I <: Int, O <: Int](cfg: SpeciationConfig)(population: List[Genome[W, I, O]])(using F: Fractional[W]): List[List[Genome[W, I, O]]] =
    population match
      case archetype :: Nil => List(List(archetype))
      case archetype :: rest =>
        val (classified, tobe) = rest.foldLeft(List(archetype) -> List.empty[Genome[W, I, O]]) {
          case ((same, rest), g) => if g.compare(archetype, cfg) < cfg.threshold then (g :: same, rest)
                                                                                  else (same, g :: rest)
        }
        classified :: classify(cfg)(tobe)
      case _ => List()

  private def constructor[W, I <: Int, O <: Int](using r: Random[IO], rr: RandomRange[IO, W], vI: ValueOf[I], vO: ValueOf[O]): GenePoolStateT[IO, Genome[W, I, O]] =
    GenePool.genome[IO, W, I, O]


  def evolve[W: Order: Numeric, I <: Int: ValueOf, O <: Int: ValueOf, S: Numeric: Ordering: Fractional](
      data: Set[(List[W], List[W])],
      transfer: W => W,
      fitnessFn: (List[List[W]], List[List[W]]) => S,
      popsize: Int,
      generations: Int,
      defaultBias: W,
      weightChance: Double,
      resetChance: Double,
      connectionChance: Double,
      nodeChance: Double,
      eliteFraction: Double,
      minScore: Option[S],
      speciationConfig: SpeciationConfig
  )(using r: Random[IO], rr: RandomRange[IO, W], f: Fractional[W]): GenePoolStateT[IO, List[(Genome[W, I, O], S)]] = {

    import GenePool.{*, given}

    // Step 0: Type alias for readability
    type GState[A] = GenePoolStateT[IO, A]

    for {

      // Step 1: Initialize population
      initialPopulation: List[Genome[W, I, O]] <- constructor[W, I, O].replicateA(popsize)

      // Step 2: Evolve generations
      finalPopulation: List[Genome[W, I, O]] <- (1 to generations).toList.foldM(initialPopulation) { (pop, gen) =>
        // Step 2a: Evaluate fitness for each genome
        val evaluatedSpecies: List[List[(Genome[W, I, O], S)]] =
          classify(speciationConfig)(pop).map { species =>
            species.map { g =>
              val outputs: List[List[W]] = data.toList.map { case (inputs, _) =>
                Individual.evaluate(g, transfer, inputs, defaultBias)
              }
              val expected: List[List[W]] = data.toList.map { case (_, exp) => exp }
              val score: S = fitnessFn(outputs, expected)
              g -> score
            }.sortBy(_._2)(Ordering[S].reverse)
          }

          
        for {
          // Diagnostic logging inside the monad
          _ <- GenePool.liftF(IO.println(s"Generation $gen: species sizes = ${evaluatedSpecies.map(_.size)}"))

          allScores = evaluatedSpecies.flatten.map(_._2)
          bestScore  = allScores.max
          worstScore = allScores.min
          avgScore   = Numeric[S].toDouble(allScores.sum) / allScores.size.toDouble

          speciesScores = evaluatedSpecies.map(s => s.map(_._2).sum)
          totalFitness = speciesScores.sum

          _ <- GenePool.liftF(IO.println(f"Generation $gen: best=$bestScore, worst=$worstScore, avg=$avgScore%.3f"))
          nextGenPerSpecies: List[List[GState[Genome[W, I, O]]]] = evaluatedSpecies.map { klass =>

            // Elites
            val elites: List[GState[Genome[W, I, O]]] = klass.take((klass.size * eliteFraction).ceil.toInt).map { case (g, _) => pure(g) }

            val breed = elites.size
            val speciesScore = klass.map(_._2).sum
            val speciesShare = Numeric[S].toDouble(speciesScore / totalFitness) * popsize
            val needed: Int = speciesShare.round.toInt - elites.size

            val children: List[GState[Genome[W, I, O]]] =
              if (needed <= 0) Nil
              else if (klass.size >= 2) {
                val breeders: List[(Genome[W, I, O], S)] = klass.take(breed)
                List.fill(needed) {
                  for {
                    // Pick two random breeders
                    i1: Int <- liftF(r.nextIntBounded(breed))
                    i2: Int <- liftF(r.nextIntBounded(breed))
                    (g1, s1) = breeders(i1)
                    (g2, s2) = breeders(i2)

                    // Determine dominant and recessive
                    (dominant, recessive) =
                      if (Ordering[S].gteq(s1, s2)) (g1, g2)
                      else (g2, g1)

                    // Crossover + mutation
                    offspring <- cross[IO, W, I, O](dominant, recessive) >>=
                                mutate[IO, W, I, O](weightChance, connectionChance, nodeChance, resetChance)
                  } yield offspring
                }
              } else if (klass.size == 1) {
                List.fill(needed)(pure(klass.head._1))
              } else Nil

            (elites ++ children).take(klass.size)
          }
          nextGen: List[Genome[W, I, O]] <- nextGenPerSpecies.flatten.sequence
        } yield nextGen
      }

      // Step 3: Final evaluation
      finalEvaluated: List[(Genome[W, I, O], S)] =
        classify(speciationConfig)(finalPopulation).map { species =>
          species.map { g =>
            val outputs: List[List[W]] = data.toList.map { case (inputs, _) =>
              Individual.evaluate(g, transfer, inputs, defaultBias)
            }
            val expected: List[List[W]] = data.toList.map { case (_, exp) => exp }
            val score: S = fitnessFn(outputs, expected)
            g -> score
          }.maxBy(_._2)
        }

      // Step 4: Filter winners by minScore
      winners: List[(Genome[W, I, O], S)] = minScore match {
        case Some(ms) => finalEvaluated.filter { case (_, s) => Ordering[S].gteq(s, ms) }
        case None     => finalEvaluated
      }

      _ = {
        println(winners)
      }

    } yield winners.sortBy(g => g._1.nodes.size)
  }

end Evolution
