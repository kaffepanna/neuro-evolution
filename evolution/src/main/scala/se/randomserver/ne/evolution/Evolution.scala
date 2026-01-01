package se.randomserver.ne.evolution

import se.randomserver.ne.genome.Genome
import cats.data.StateT
import cats.syntax.all.{*, given}
import cats.Monad
import se.randomserver.ne.genome.SpeciationConfig
import cats.data.ReaderT
import cats.Applicative
import scala.math.Fractional.Implicits.infixFractionalOps
import cats.effect.std.Random
import se.randomserver.ne.genome.GenePool.cross
import se.randomserver.ne.genome.GenePool.mutate
import se.randomserver.ne.genome.RandomRange
import cats.Order
import se.randomserver.ne.genome.GenePool.{*, given}
import se.randomserver.ne.genome.GenePool
import se.randomserver.ne.evolution.EvolutionConfig
import se.randomserver.ne.evaluator.Compiler
import se.randomserver.ne.evaluator.Runner
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork

object Evolution {
  trait Epsilon[W] {
    def zero: W
  }

  type GenomeId = Int
  type SpeciesId = Int

  given Epsilon[Double] = new Epsilon[Double] {
    def zero: Double = 1e-8
  }

  object Epsilon {
    def apply[W](using e: Epsilon[W]): W = e.zero
  }
  
  case class EvaluatedGenome[W, I <: Int, O <: Int, S](
    genome: Genome[W, I, O],
    rawFitness: Option[S],
    adjustedFitness: Option[S],
    compiled: Option[CompiledNetwork[W]]
  )

  case class Species[W, I <: Int, O <: Int](
    id: Int,
    representative: GenomeId,
    members: Vector[GenomeId],
    age: Int
  )

  case class EvolutionState[W, I <: Int, O <: Int, S](
    species: Vector[Species[W, I, O]] = Vector.empty[Species[W, I, O]],
    population: Map[GenomeId, Genome[W, I, O]] = Map.empty[GenomeId, Genome[W, I, O]],
    fitness: Map[GenomeId, S] = Map.empty[GenomeId, S],
    adjustedFitness: Map[GenomeId, S] = Map.empty[GenomeId, S],
    generation: Int = 0,
    nextSpeciesId: SpeciesId = 0,
    nextGenomeId: GenomeId = 0
  ) {
    type Weight = W
    type Inputs = I
    type Outputs = O
    type Score = S
  }

  case class EvolutionEnv[W, S](
    data: List[(List[W], List[W])],
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
    recurrentSteps: Int,
    speciationConfig: SpeciationConfig
  )

  opaque type EvolutionStateT[F[_], W, I <: Int, O <: Int, S, A] = StateT[F, EvolutionState[W, I, O, S], A]

  opaque type EvolutionT[F[_], W, I <: Int, O <: Int, S, A] = ReaderT[
    EvolutionStateT[GenePoolStateT[F, *], W, I, O, S, *],
    EvolutionEnv[W, S],
    A
  ]

  extension [F[_], W, I <: Int, O <: Int, S, A](et: EvolutionT[F, W, I, O, S, A])
    def runEvolution(env: EvolutionEnv[W, S], state: EvolutionState[W, I, O, S], gp: GenePool)(using F: Monad[F]) =
      et.run(env).run(state).run(gp)

  given [F[_], W, I <: Int, O <: Int, S](using ap: Monad[StateT[F, EvolutionState[W, I, O, S], _]]): Monad[EvolutionStateT[F, W, I, O, S, _]] = ap
  given [F[_], W, I <: Int, O <: Int, S](using ap: Monad[ReaderT[EvolutionStateT[GenePoolStateT[F, *], W, I, O, S, *], EvolutionEnv[W, S], _]]): Monad[EvolutionT[F, W, I, O, S, _]] = ap

  object EvolutionT {
    def pure[F[_]: Monad, W, I <: Int, O  <: Int, S, A](a: A): EvolutionT[F, W, I, O, S, A] =
      ReaderT.pure[
        EvolutionStateT[GenePoolStateT[F, *], W, I, O, S, *],
        EvolutionEnv[W, S],
        A
      ](a)

    def ask[F[_]: Monad, W, I <: Int, O <: Int, S] = getEnv[F, W, I, O, S]

    def getEnv[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, EvolutionEnv[W, S]] =
      ReaderT.ask[
        EvolutionStateT[GenePoolStateT[F, *], W, I, O, S, *],
        EvolutionEnv[W, S]
      ]

    def getState[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, EvolutionState[W, I, O, S]] =
      ReaderT.liftF(
        StateT.get[
          GenePoolStateT[F, *],
          EvolutionState[W, I, O, S]
        ]
      )

    def modifyState[F[_]: Monad, W, I <: Int, O <: Int, S](
      f: EvolutionState[W, I, O, S] => EvolutionState[W, I, O, S]
    ): EvolutionT[F, W, I, O, S, Unit] =
      ReaderT.liftF(
        StateT.modify[
          GenePoolStateT[F, *],
          EvolutionState[W, I, O, S]
        ](f)
      )

    def liftF[F[_]: Monad, W, I <: Int, O <: Int, S, A](fa: F[A])
      : EvolutionT[F, W, I, O, S, A] =
      ReaderT.liftF(StateT.liftF(GenePool.liftF(fa)))

    def liftGenePool[F[_]: Monad, W, I <: Int, O <: Int, S, A](gp: GenePoolStateT[F, A]): EvolutionT[F, W, I, O, S, A] =
      ReaderT.liftF(StateT.liftF(gp))
  }

  def nextSpeciesId[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, SpeciesId] =
    EvolutionT.getState[F, W, I, O, S] >>= { state =>
      val nextId = state.nextSpeciesId + 1
      EvolutionT.modifyState[F, W, I, O ,S] { state => state.copy(nextSpeciesId = nextId) } 
        >> EvolutionT.pure(nextId)
    }

  def nextGenomeId[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, GenomeId] = for {
    state <- EvolutionT.getState[F, W, I, O, S]
    nextId = state.nextGenomeId + 1
    _ <- EvolutionT.modifyState[F, W, I, O, S] { state => state.copy(nextGenomeId = nextId) }
  } yield nextId

  def evaluateFitness[F[_]: Monad, W: Numeric: Fractional, I <: Int, O <: Int, S: Ordering]: EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]
    expected = env.data.map(_._2)
    state <- EvolutionT.getState[F, W, I, O, S]
    scores = state.population.map {
      case (id, genome) =>
        val compiled = Compiler.compileGenome(genome, env.transfer)
        val outputs = env.data.map(_._1.toVector).map { input =>
          val in = compiled.inputs.toVector.sorted.zip(input).toMap
          Runner.evalNetwork(compiled, in, env.defaultBias, env.recurrentSteps).toList
        }
        id -> env.fitnessFn(outputs, expected)
    }
    _ <- EvolutionT.modifyState[F, W, I, O, S](state => state.copy(fitness = scores))
  } yield ()

  def adjustFitnessSharing[F[_]: Monad, W, I <: Int, O <: Int, S: Fractional]: EvolutionT[F, W, I, O, S, Unit] = for {
    state <- EvolutionT.getState[F, W, I, O, S]
    f = summon[Fractional[S]]
    adjusted = state.fitness.map {
      case (genomeId, score) =>
        val speciesSize = state.species.find(_.members.contains(genomeId)) match {
          case Some(species) => species.members.size
          case None => throw new IllegalStateException(s"Genome: $genomeId not member of any species")
        }
        genomeId -> f.div(state.fitness(genomeId), f.fromInt(speciesSize))
    }
    _ <- EvolutionT.modifyState[F, W, I, O, S](state => state.copy(adjustedFitness = adjusted))
  } yield ()

  def cullSpecies[F[_]: Monad, W, I <: Int, O <: Int, S: Ordering]
  : EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]
    state <- EvolutionT.getState[F, W, I, O, S]
    updated = state.species.map[Species[W, I, O]] { species =>
      val sorted = species.members.sortBy(state.fitness)(Ordering[S].reverse)
      val keepN = math.max(1, math.ceil(sorted.size * env.eliteFraction).toInt)
      species.copy(members = sorted.take(keepN))
    }
    _ <- EvolutionT.modifyState[F, W, I, O ,S](state => state.copy(species = updated))
  } yield ()

  def allocateOffspringPerSpecies[
    F[_]: Monad,
    W,
    I <: Int,
    O <: Int,
    S: Fractional: Numeric: Epsilon
  ]: EvolutionT[F, W, I, O, S, Map[SpeciesId, Int]] =
    for {
      env   <- EvolutionT.getEnv[F, W, I, O, S]
      state <- EvolutionT.getState[F, W, I, O, S]
      // 1. Compute total adjusted fitness
      speciesFitnesses = state.species.map { species =>
        val fitnessSum = species.members.map(state.adjustedFitness).foldLeft(Fractional[S].zero)(_ + _)
        species.id -> fitnessSum
      }

      minFitness = speciesFitnesses.map(_._2).min

      shifted = if (Fractional[S].lteq(minFitness, Fractional[S].zero)) speciesFitnesses.map { case (id, f) => id -> (f - minFitness + Epsilon[S]) }
                else speciesFitnesses

      totalFitness =
        shifted.map(_._2).foldLeft(Fractional[S].zero)(_ + _)

      // 2. Raw allocation
      rawAllocations =
        if (totalFitness == Fractional[S].zero)
          // fallback: uniform distribution
          state.species.map(sp => sp.id -> 1)
        else
          shifted.map { case (id, fit) =>
            val share =
              Fractional[S].toDouble(fit / totalFitness)
            val count =
              math.round(share * env.popsize).toInt
            id -> count
          }

      // 3. Normalize to exact population size
      normalized = {
        val total = rawAllocations.map(_._2).sum
        val diff  = env.popsize - total

        if (diff == 0) rawAllocations
        else {
          // distribute remainder to best species
          val sorted =
            rawAllocations.sortBy(-_._2)

          sorted.zipWithIndex.map { case ((id, c), idx) =>
            if (idx < math.abs(diff))
              id -> (c + diff.sign)
            else
              id -> c
          }
        }
      }

    } yield normalized.toMap

  def reproduce[F[_]: Monad, W: Numeric: Order, I <: Int, O <: Int, S: Ordering: Fractional](
    offspringCounts: Map[GenomeId, Int]
  )(using R: Random[F], RR: RandomRange[F, W]): EvolutionT[F, W, I, O, S, Map[GenomeId, Genome[W, I, O]]] = for {
      env   <- EvolutionT.getEnv[F, W, I, O, S]
      state <- EvolutionT.getState[F, W, I, O, S]
      children <- state.species.flatMap { species =>
        val sortedMembers = species.members.sortBy(state.adjustedFitness)(Ordering[S].reverse)
        val count = offspringCounts.getOrElse(species.id, 0)
        val eliteCount = math.max(1, (species.members.size * env.eliteFraction).ceil.toInt)
        val elites = sortedMembers.take(eliteCount).map(id => id -> state.population(id))
        val breeders = sortedMembers.take(math.max(2, sortedMembers.size / 2)).map(id => id -> state.population(id))

        val childrenCount = math.max(0, count - elites.size)
        val children = Vector.fill(childrenCount) {
          for {
            id1p1 <- EvolutionT.liftF(R.nextIntBounded(breeders.size).map(breeders(_)))
            id2p2 <- EvolutionT.liftF(R.nextIntBounded(breeders.size).map(breeders(_)))
            (id1, p1) = id1p1
            (id2, p2) = id2p2
            (dominant, recessive) = if (Ordering[S].gteq(state.adjustedFitness(id1), state.adjustedFitness(id2))) (p1, p2)
                                    else (p1, p2)
            child <- EvolutionT.liftGenePool[F, W, I, O, S, Genome[W, I, O]] {
              cross[F,W,I,O](dominant, recessive) >>= mutate[F, W, I, O](env.weightChance, env.connectionChance, env.nodeChance, env.resetChance)
            }
            newId <- nextGenomeId
          } yield newId -> child
        }
        children ++ elites.map(EvolutionT.pure)
    }.sequence
  } yield children.toMap

  def speciate[F[_]: Monad, W: Fractional, I <: Int, O <: Int, S](
    newPopulation: Map[GenomeId, Genome[W, I, O]]
  ): EvolutionT[F, W, I, O, S, Unit] = for {
    env   <- EvolutionT.getEnv[F, W, I, O, S]
    state <- EvolutionT.getState[F, W, I, O, S]

    // Reset members but keep species identity
    clearedSpecies =
      state.species.map[Species[W, I, O]](s => s.copy(members = Vector.empty))

    // Assign genomes to existing species if possible
    (assignedSpecies, unassigned) =
      newPopulation.foldLeft((clearedSpecies, Vector.empty[GenomeId])) {
        case ((speciesAcc, unassignedAcc), (genomeId, genome)) =>
          speciesAcc.indexWhere { s =>
            genome.compare(state.population(s.representative), env.speciationConfig) < env.speciationConfig.threshold
          } match {
            case -1 =>
              (speciesAcc, unassignedAcc :+ genomeId)

            case idx =>
              val s = speciesAcc(idx)
              val updated: Species[W, I, O] =
                s.copy(members = s.members :+ genomeId)
              (speciesAcc.updated(idx, updated), unassignedAcc)
          }
      }

    clusteredNewSpecies = {
      def cluster(
        remaining: Vector[GenomeId],
        acc: Vector[Species[W, I, O]],
        nextId: SpeciesId
      ): Vector[Species[W, I, O]] =
        remaining match {
          case Vector() => acc

          case Vector(rep, rest*) =>
            val (same, others) =
              rest.partition(g =>
                newPopulation(g).compare(newPopulation(rep), env.speciationConfig) < env.speciationConfig.threshold
              )

            val members = (rep +: same)

            val species =
              Species[W, I, O](
                id = nextId,
                representative = rep,
                members = members.toVector,
                age = 0
              )

            cluster(others.toVector, acc :+ species, nextId + 1)
        }

      val startId = state.nextSpeciesId

      cluster(unassigned, Vector.empty, startId)
    }

    // Drop empty species
    survivingSpecies =
      assignedSpecies.filter(_.members.nonEmpty) ++ clusteredNewSpecies

    _ <- EvolutionT.modifyState[F, W, I, O, S](
      _.copy(species = survivingSpecies, nextSpeciesId = state.nextSpeciesId + clusteredNewSpecies.size, population = newPopulation)
    )

  } yield ()

  def reassignRepresentatives[F[_]: Monad, W, I <: Int, O <: Int, S: Ordering: Fractional]: EvolutionT[F, W, I, O, S, Unit] =
    for {
      state <- EvolutionT.getState[F, W, I, O, S]
      newSpecies = state.species.map { species =>
        if (species.members.isEmpty) species
        else {
          // Pick the highest-fitness member as representative
          val bestMember = species.members.maxBy(state.adjustedFitness.getOrElse(_, Fractional[S].zero))(Ordering[S].reverse)
          species.copy(representative = bestMember)
        }
      }
      _ <- EvolutionT.modifyState[F, W, I, O, S](_.copy(species = newSpecies))
    } yield ()

  def debug[F[_]: Monad: Random, W: Fractional: Order, I <: Int, O <: Int, S: Fractional: Ordering]: EvolutionT[F, W, I, O, S, Unit] = for {
    state <- EvolutionT.getState[F, W, I, O, S]
    speciesFitnesses = state.species.map { sp =>
      val fitnessSum =
        sp.members.map(state.adjustedFitness).foldLeft(Fractional[S].zero)(_ + _)
      sp.id -> fitnessSum 
    }.toMap
    currentGen = state.generation
    _ = {
      println(s"Generation: ${currentGen}")
      state.species.foreach { species =>
        val avgSize = species.members.map(state.population).foldLeft(0)((n, genome) => n + genome.nodes.size) / species.members.size
        val avgConnections = species.members.map(state.population).foldLeft(0)((n, genome) => n + genome.genes.size) / species.members.size
        println(s"\tSpecies: ${species.id} - age: ${species.age}, size: ${species.members.size}, score: ${String.format("%.2f", speciesFitnesses(species.id))}, nodes: ${avgSize}, conns: ${avgConnections}")
      }
    }
  } yield ()

  def evolveStep[F[_]: Monad: Random, W: Fractional: Order, I <: Int, O <: Int, S: Fractional: Ordering: Epsilon](using RandomRange[F, W]): EvolutionT[F, W, I, O, S, Unit] = for {
    _ <- evaluateFitness
    _ <- adjustFitnessSharing
    _ <- debug
    _ <- cullSpecies
    offspringPlan <- allocateOffspringPerSpecies
    newPop <- reproduce(offspringPlan)
    _ <- speciate(newPop)
    _ <- reassignRepresentatives
    _ <- EvolutionT.modifyState[F, W, I, O, S] { state =>
      val ageIncremented = state.species.map[Species[W, I, O]](s => s.copy(age = s.age + 1))
      state.copy(generation = state.generation + 1, species = ageIncremented)
    }
  } yield ()

  def evolve[F[_]: Monad: Random, W: Fractional: Order, I <: Int: ValueOf, O <: Int: ValueOf, S: Fractional: Ordering: Epsilon](using RandomRange[F, W]): EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]
    genomes <- EvolutionT.liftGenePool[F, W, I, O, S, Genome[W, I, O]](genome[F, W, I, O]()).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => EvolutionT.pure[F, W, I, O, S, (GenomeId, Genome[W, I, O])](id -> genome))
    }.sequence
    _ <- speciate(initialPop.toMap)
    _ <- List.fill(env.generations)(()).traverse_(_ => evolveStep[F, W, I, O, S])
    _ <- evaluateFitness
  } yield ()
    

  
}