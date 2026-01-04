package se.randomserver.ne.evolution

import se.randomserver.ne.genome.Genome
import cats.data.StateT
import cats.Monad
import cats.data.ReaderT
import cats.Applicative
import cats.effect.std.Random
import cats.mtl.Stateful
import cats.mtl.Ask
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*
import cats.syntax.foldable.*
import spire.compat.ordering
import spire.syntax.all.*
import spire.math.*
import spire.implicits.given
import spire.algebra.{Order, Ring, Field, Monoid}
import se.randomserver.ne.genome.SpeciationConfig
import se.randomserver.ne.genome.GenePool.cross
import se.randomserver.ne.genome.GenePool.mutate
import se.randomserver.ne.genome.RandomRange
import se.randomserver.ne.genome.GenePool.{*, given}
import se.randomserver.ne.genome.GenePool
import se.randomserver.ne.evolution.EvolutionConfig
import se.randomserver.ne.evaluator.Compiler
import se.randomserver.ne.evaluator.Runner
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork
import algebra.ring.Semiring
import se.randomserver.ne.genome.HasGenePool
import algebra.ring.Signed


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
  
  case class EvaluatedGenome[W, S](
    genome: Genome[W],
    rawFitness: Option[S],
    adjustedFitness: Option[S],
    compiled: Option[CompiledNetwork[W]]
  )

  case class Species[W](
    id: Int,
    representative: GenomeId,
    members: Vector[GenomeId],
    age: Int
  )

  final case class EvolutionState[W, S](
    species: Vector[Species[W]] = Vector.empty[Species[W]],
    population: Map[GenomeId, Genome[W]] = Map.empty[GenomeId, Genome[W]],
    fitness: Map[GenomeId, S] = Map.empty[GenomeId, S],
    adjustedFitness: Map[GenomeId, S] = Map.empty[GenomeId, S],
    generation: Int = 0,
    nextSpeciesId: SpeciesId = 0,
    nextGenomeId: GenomeId = 0
  )

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

  type HasEvolutionEnv[F[_], W, S] = Ask[F, EvolutionEnv[W, S]]

  object HasEvolutionEnv:
    def apply[F[_], W, S](using he: HasEvolutionEnv[F, W, S]) = he

  type HasEvolutionState[F[_], W, S] = Stateful[F, EvolutionState[W, S]]

  object HasEvolutionState:
    def apply[F[_], W, S](using hs: HasEvolutionState[F, W, S]) = hs

  def modifyState[F[_], W, S, B](fn: EvolutionState[W, S] => F[(EvolutionState[W, S], B)])(using Monad[F], HasEvolutionState[F, W, S]): F[B] = for 
    state <- HasEvolutionState[F, W, S].get
    ret <- fn(state)
    (modified, b) = ret
    _ <- HasEvolutionState[F, W, S].set(modified)
  yield b

  def getState[F[_], W, S](using hs: HasEvolutionState[F, W, S]): F[EvolutionState[W, S]] = hs.get
  def setState[F[_], W, S](using hs: HasEvolutionState[F, W, S]) = hs.set
  def getEnv[F[_], W, S](using he: HasEvolutionEnv[F, W, S]): F[EvolutionEnv[W, S]] = he.ask


  def nextSpeciesId[F[_], W, S](using Monad[F], HasEvolutionState[F, W, S]): F[SpeciesId] = modifyState { (state: EvolutionState[W, S]) =>
      val nextId = state.nextSpeciesId + 1
      (state.copy(nextSpeciesId = nextId) -> nextId).pure
  }

  def nextGenomeId[F[_], W, S](using Monad[F], HasEvolutionState[F, W, S]): F[GenomeId] = modifyState { (state: EvolutionState[W, S]) =>
    val nextId = state.nextGenomeId + 1
    (state.copy(nextGenomeId = nextId), nextId).pure   
  }

  def evaluateFitness[F[_]: Monad, W: Semiring, S: Order](using HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S]): F[Unit] = for {
    env <- getEnv[F, W, S]
    expected = env.data.map(_._2)
    state <- HasEvolutionState[F, W, S].get
    scores = state.population.map {
      case (id, genome) =>
        val compiled = Compiler.compileGenome(genome, env.transfer)
        val outputs = env.data.map(_._1.toVector).map { input =>
          val in = compiled.inputs.toVector.sorted.zip(input).toMap
          Runner.evalNetwork(compiled, in, env.defaultBias, env.recurrentSteps).toList
        }
        id -> env.fitnessFn(outputs, expected)
    }
    _ <- setState(state.copy(fitness = scores))
  } yield ()

  def adjustFitnessSharing[F[_]: Monad, W, S: Field](using HasEvolutionState[F, W, S]): F[Unit] = for {
    state <- getState[F, W, S]
    adjusted = state.fitness.map {
      case (genomeId, score) =>
        val speciesSize = state.species.find(_.members.contains(genomeId)) match {
          case Some(species) => species.members.size
          case None => throw new IllegalStateException(s"Genome: $genomeId not member of any species")
        }
        genomeId -> state.fitness(genomeId) / speciesSize
    }
    _ <- setState(state.copy(adjustedFitness = adjusted))
  } yield ()

  def cullSpecies[F[_]: Monad, W, S: Order](using HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S]): F[Unit] = for {
    env <- getEnv[F, W, S]
    state <- getState[F, W, S]
    updated = state.species.map[Species[W]] { species =>
      val sorted = species.members.sortBy(state.fitness).reverse
      val keepN = math.max(1, math.ceil(sorted.size * env.eliteFraction).toInt)
      species.copy(members = sorted.take(keepN))
    }
    _ <- setState(state.copy(species = updated))
  } yield ()

  def allocateOffspringPerSpecies[F[_]: Monad, W, S: Numeric: Order](using HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S]): F[Map[SpeciesId, Int]] =
    for {
      env   <- getEnv[F, W, S]
      state <- getState[F, W, S]
      // 1. Compute total adjusted fitness
      speciesFitnesses = state.species.map { species =>
        val fitnessSum = species.members.map(state.adjustedFitness).foldLeft(Numeric[S].zero)(_ + _)
        species.id -> fitnessSum
      }

      minFitness = speciesFitnesses.map(_._2).min

      shifted = if (minFitness <= Numeric[S].zero) speciesFitnesses.map { case (id, f) => id -> (f - minFitness) }
                else speciesFitnesses

      totalFitness =
        shifted.map(_._2).foldLeft(Numeric[S].zero)(_ + _)

      // 2. Raw allocation
      rawAllocations =
        if (totalFitness == Numeric[S].zero)
          // fallback: uniform distribution
          state.species.map(sp => sp.id -> 1)
        else
          shifted.map { case (id, fit) =>
            val share = (fit / totalFitness) * env.popsize
            val count = Numeric[S].floor((share)).toInt
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

  def reproduce[F[_]: Monad, W: Semiring: Order, S: Order](
    offspringCounts: Map[SpeciesId, Int]
  )(using Random[F], RandomRange[F, W], HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S], HasGenePool[F]): F[Map[GenomeId, Genome[W]]] = for {
      env   <- getEnv[F, W, S]
      state <- getState[F, W, S]
      children <- state.species.flatMap { species =>
        val sortedMembers = species.members.sortBy(state.adjustedFitness).reverse
        val count = offspringCounts.getOrElse(species.id, 0)
        val eliteCount = math.max(1, (species.members.size * env.eliteFraction).ceil.toInt)
        val elites = sortedMembers.take(eliteCount).map(id => id -> state.population(id))
        val breeders = sortedMembers.take(math.max(2, sortedMembers.size / 2)).map(id => id -> state.population(id))
        if (count <= 0)
          Vector.empty
        else {
          val childrenCount = math.max(0, count - elites.size)
          val children = Vector.fill(childrenCount) {
            for {
              id1p1 <- Random[F].nextIntBounded(breeders.size).map(breeders(_))
              id2p2 <- Random[F].nextIntBounded(breeders.size).map(breeders(_))
              (id1, p1) = id1p1
              (id2, p2) = id2p2
              (dominant, recessive) = if (state.adjustedFitness(id1) >= state.adjustedFitness(id2)) (p1, p2)
                                      else (p1, p2)
              child <- 
                cross[F,W](dominant, recessive) >>= mutate[F, W](env.weightChance, env.connectionChance, env.nodeChance, env.resetChance)

              newId <- nextGenomeId
            } yield newId -> child
          }
          children ++ elites.map(Monad[F].pure)
        }
    }.sequence
  } yield children.toMap

  def speciate[F[_]: Monad, W: Field: Signed: Order, S](
    newPopulation: Map[GenomeId, Genome[W]]
  )(using HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S]): F[Unit] = for {
    env   <- getEnv[F, W, S]
    state <- getState[F, W, S]

    // Reset members but keep species identity
    clearedSpecies =
      state.species.map[Species[W]](s => s.copy(members = Vector.empty))

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
              val updated: Species[W] =
                s.copy(members = s.members :+ genomeId)
              (speciesAcc.updated(idx, updated), unassignedAcc)
          }
      }

    clusteredNewSpecies = {
      def cluster(
        remaining: Vector[GenomeId],
        acc: Vector[Species[W]],
        nextId: SpeciesId
      ): Vector[Species[W]] =
        remaining match {
          case Vector() => acc

          case Vector(rep, rest*) =>
            val (same, others) =
              rest.partition(g =>
                newPopulation(g).compare(newPopulation(rep), env.speciationConfig) < env.speciationConfig.threshold
              )

            val members = (rep +: same)

            val species =
              Species[W](
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

    _ <- setState[F, W, S](
      state.copy(species = survivingSpecies, nextSpeciesId = state.nextSpeciesId + clusteredNewSpecies.size, population = newPopulation)
    )

  } yield ()

  def reassignRepresentatives[F[_]: Monad, W, S: Order: Semiring](using HasEvolutionState[F, W, S], Random[F]): F[Unit] =
    for {
      state <- getState[F, W, S]
      newSpecies <- state.species.map { species =>
        if (species.members.isEmpty) species.pure[F]
        else {
          // Pick the highest-fitness member as representative
          //val bestMember = species.members.maxBy(state.adjustedFitness.getOrElse(_, Semiring[S].zero))(using Ordering[S].reverse)
          Random[F].betweenInt(0, species.members.size) >>= { rep =>
            species.copy(representative = species.members(rep)).pure
          }
        }
      }.sequence
      _ <- setState(state.copy(species = newSpecies))
    } yield ()

  def debug[F[_]: Monad: Random, W: Order, S: Semiring: Order](using HasEvolutionState[F, W, S]): F[Unit] = for {
    state <- getState[F, W, S]
    speciesFitnesses = state.species.map { sp =>
      val fitnessSum =
        sp.members.map(state.adjustedFitness).foldLeft(Semiring[S].zero)(_ + _)
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

  def evolveStep[F[_]: Monad: Random, W: Field: Signed: Semiring: Order, S: Field: Numeric: Semiring: Order](using RandomRange[F, W], HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S], HasGenePool[F]): F[Unit] = for {
    _ <- evaluateFitness[F, W, S]
    _ <- adjustFitnessSharing[F, W, S]
    _ <- debug[F, W, S]
    _ <- cullSpecies[F, W, S]
    offspringPlan <- allocateOffspringPerSpecies[F, W, S]
    newPop <- reproduce[F, W, S](offspringPlan)
    _ <- speciate(newPop)
    _ <- reassignRepresentatives[F, W, S]
    _ <- modifyState[F, W, S, Unit] { state =>
      val ageIncremented = state.species.map[Species[W]](s => s.copy(age = s.age + 1))
      (state.copy(generation = state.generation + 1, species = ageIncremented) -> ()).pure
    }
  } yield ()

  def evolve[F[_]: Monad: Random, W: Field: Signed: Semiring: Order, I <: Int : ValueOf, O <: Int: ValueOf, S: Field: Numeric: Semiring: Order](using RandomRange[F, W], HasEvolutionEnv[F, W, S], HasEvolutionState[F, W, S], HasGenePool[F]): F[Unit] = for {
    env <- getEnv[F, W, S]
    genomes <- genome[F, W](valueOf[I], valueOf[O]).replicateA(env.popsize)
    initialPop <- genomes.map { genome =>
        nextGenomeId >>= (id => Monad[F].pure(id -> genome))
    }.sequence
    _ <- speciate(initialPop.toMap)
    _ <- List.fill(env.generations)(()).traverse_(_ => evolveStep[F, W, S])
    _ <- evaluateFitness[F, W, S]
  } yield ()
    

  
}