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
import se.randomserver.ne.RandomRange
import cats.Order
import se.randomserver.ne.genome.GenePool.{*, given}
import se.randomserver.ne.genome.GenePool
import se.randomserver.ne.evolution.EvolutionConfig
import se.randomserver.ne.evaluator.Compiler
import se.randomserver.ne.evaluator.Runner
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork

object Evolution {
  case class EvaluatedGenome[W, I <: Int, O <: Int, S](
    genome: Genome[W, I, O],
    rawFitness: Option[S],
    adjustedFitness: Option[S],
    compiled: Option[CompiledNetwork[W]]
  )

  case class Species[W, I <: Int, O <: Int, S](
    id: Long,
    representative: Genome[W, I, O],
    members: Vector[EvaluatedGenome[W, I, O, S]],
    age: Int
  )

  case class SpeciesOffspring(
    speciesId: Long,
    count: Int
  )

  case class EvolutionState[W, I <: Int, O <: Int, S](
    species: Vector[Species[W, I, O, S]],
    generation: Int,
    nextSpeciesId: Long = 0
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

  opaque type EvolutionStateT[F[_], W, I <: Int, O <: Int, S, A] = StateT[F, EvolutionState[W, I, O, S], A]

  opaque type EvolutionT[F[_], W, I <: Int, O <: Int, S, A] = ReaderT[
    EvolutionStateT[GenePoolStateT[F, *], W, I, O, S, *],
    EvolutionEnv[W, S],
    A
  ]

  extension [F[_], W, I <: Int, O <: Int, S, A](et: EvolutionT[F, W, I, O, S, A])
    def runEvolution(env: EvolutionEnv[W, S], state: EvolutionState[W, I, O, S], gp: GenePool)(using F: Monad[F]) =
      et.run(env).run(state).run(gp)

  given [F[_], W, I <: Int, O <: Int, S](using ap: Applicative[StateT[F, EvolutionState[W, I, O, S], _]]): Applicative[EvolutionStateT[F, W, I, O, S, _]] = ap
  given [F[_], W, I <: Int, O <: Int, S](using ap: Monad[StateT[F, EvolutionState[W, I, O, S], _]]): Monad[EvolutionStateT[F, W, I, O, S, _]] = ap

  object EvolutionT {
    def pure[F[_]: Monad, W, I <: Int, O  <: Int, S, A](a: A): EvolutionT[F, W, I, O, S, A] =
      ReaderT.pure(a)
    def getEnv[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, EvolutionEnv[W, S]] =
      ReaderT.ask

    def getState[F[_]: Monad, W, I <: Int, O <: Int, S]: EvolutionT[F, W, I, O, S, EvolutionState[W, I, O, S]] =
      ReaderT.liftF(StateT.get)

    def modifyState[F[_]: Monad, W, I <: Int, O <: Int, S](
      f: EvolutionState[W, I, O, S] => EvolutionState[W, I, O, S]
    ): EvolutionT[F, W, I, O, S, Unit] =
      ReaderT.liftF(StateT.modify(f))

    def liftF[F[_]: Monad, W, I <: Int, O <: Int, S, A](fa: F[A])
      : EvolutionT[F, W, I, O, S, A] =
      ReaderT.liftF(StateT.liftF(GenePool.liftF(fa)))

    def liftGenePool[F[_]: Monad, W, I <: Int, O <: Int, S, A](gp: GenePoolStateT[F, A]): EvolutionT[F, W, I, O, S, A] =
      ReaderT.liftF(StateT.liftF(gp))
    }

  def evaluateFitness[F[_]: Monad, W: Numeric: Fractional, I <: Int, O <: Int, S: Ordering]: EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]
    expected = env.data.map(_._2)
    _ <- EvolutionT.modifyState[F, W, I, O, S] { state =>
      val updatedSpecies = state.species.map { species =>
        val evaluatedMembers = species.members.map {
          case egenome @ EvaluatedGenome(genome, rawFitness, adjustedFitness, compiledO) => {
            val compiled = compiledO.getOrElse(Compiler.compileGenome(genome, env.transfer))
            val outputs = env.data.map { case (inputs, _) =>
              val in =  compiled.inputs.zip(inputs).toMap
              val evaluation = Runner.evalNetwork(compiled, in, env.defaultBias, env.recurrentSteps)
              evaluation.toList
            }
            
            val score = env.fitnessFn(outputs, expected)
            egenome.copy(rawFitness = Some(score), compiled = Some(compiled))
          }
        }
        species.copy(members = evaluatedMembers)
      }
      state.copy(species = updatedSpecies)
    }
  } yield ()

  def adjustFitnessSharing[F[_]: Monad, W, I <: Int, O <: Int, S: Fractional]: EvolutionT[F, W, I, O, S, Unit] =
    EvolutionT.modifyState[F, W, I, O, S] { state =>
      val F = summon[Fractional[S]]
      val updatedSpecies = state.species.map { species =>
        val size = species.members.size
        val updatedMembers = species.members.map { eg =>
          eg.rawFitness match {
            case Some(s) if size > 0 => eg.copy(adjustedFitness = Some(F.div(s, F.fromInt(size))))
            case _ => eg.copy(adjustedFitness = None)
          }
        }
        species.copy(members = updatedMembers)
      }
      state.copy(species = updatedSpecies)
    }

  def cullSpecies[F[_]: Monad, W, I <: Int, O <: Int, S: Ordering]
  : EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]

    _ <- EvolutionT.modifyState[F, W, I, O, S] { state =>
      val updatedSpecies = state.species.map { species =>
        val evaluated =
          species.members.collect {
            case eg @ EvaluatedGenome(_, Some(_), _, _) => eg
          }

        if (evaluated.isEmpty) {
          // No evaluated members — keep species as-is
          species
        } else {
          val sorted =
            evaluated.sortBy(_.rawFitness)(Ordering[Option[S]].reverse)

          val keepCount =
            math.max(1, math.ceil(sorted.size * env.eliteFraction).toInt)

          val survivors =
            sorted.take(keepCount)

          species.copy(
            members = survivors
          )
        }
      }

      state.copy(species = updatedSpecies)
    }
  } yield ()

  def allocateOffspringPerSpecies[
    F[_]: Monad,
    W,
    I <: Int,
    O <: Int,
    S: Fractional
  ]: EvolutionT[F, W, I, O, S, Map[Long, Int]] =
    for {
      env   <- EvolutionT.getEnv[F, W, I, O, S]
      state <- EvolutionT.getState[F, W, I, O, S]

      // 1. Compute total adjusted fitness
      speciesFitnesses = state.species.map { sp =>
        val fitnessSum =
          sp.members.flatMap(_.adjustedFitness).foldLeft(Fractional[S].zero)(_ + _)
        sp.id -> fitnessSum
      }

      totalFitness =
        speciesFitnesses.map(_._2).foldLeft(Fractional[S].zero)(_ + _)

      // 2. Raw allocation
      rawAllocations =
        if (totalFitness == Fractional[S].zero)
          // fallback: uniform distribution
          state.species.map(sp => sp.id -> 1)
        else
          speciesFitnesses.map { case (id, fit) =>
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
    offspringCounts: Map[Long, Int]
)(using R: Random[F], RR: RandomRange[F, W]): EvolutionT[F, W, I, O, S, Vector[Genome[W, I, O]]] = for {
  env   <- EvolutionT.getEnv[F, W, I, O, S]
  state <- EvolutionT.getState[F, W, I, O, S]
  children <- state.species.flatMap { species =>
      val sortedMembers = species.members.sortBy(_.adjustedFitness)(Ordering[Option[S]].reverse)
      val count = offspringCounts.getOrElse(species.id, 0)
      val eliteCount = math.max(1, (species.members.size * env.eliteFraction).ceil.toInt)
      val elites = sortedMembers.take(eliteCount).map(_.genome)
      val breeders = sortedMembers.take(math.max(2, sortedMembers.size / 2))

      val childrenCount = math.max(0, count - elites.size)
      val children = Vector.fill(childrenCount) {
        for {
          p1 <- EvolutionT.liftF(R.nextIntBounded(breeders.size).map(breeders(_)))
          p2 <- EvolutionT.liftF(R.nextIntBounded(breeders.size).map(breeders(_)))
          (dominant, recessive) = if (Ordering[S].gteq(p1.adjustedFitness.get, p2.adjustedFitness.get)) (p1.genome, p2.genome)
                                  else (p2.genome, p1.genome)
          child <- EvolutionT.liftGenePool[F, W, I, O, S, Genome[W, I, O]] {
            cross[F,W,I,O](dominant, recessive) >>= mutate[F, W, I, O](env.weightChance, env.connectionChance, env.nodeChance, env.resetChance)
          }
        } yield child
      }
      children ++ elites.map(EvolutionT.pure)
    }.sequence
  } yield children

  def reassignRepresentatives[F[_]: Monad, W, I <: Int, O <: Int, S: Ordering]: EvolutionT[F, W, I, O, S, Unit] =
    for {
      state <- EvolutionT.getState[F, W, I, O, S]
      newSpecies = state.species.map { species =>
        if (species.members.isEmpty) species
        else {
          // Pick the highest-fitness member as representative
          val bestMember = species.members.maxBy(_.adjustedFitness)(Ordering[Option[S]].reverse)
          species.copy(representative = bestMember.genome)
        }
      }
      _ <- EvolutionT.modifyState[F, W, I, O, S](_.copy(species = newSpecies))
    } yield ()

  def speciate[F[_]: Monad, W: Fractional, I <: Int, O <: Int, S](
    newPopulation: Vector[Genome[W, I, O]]
  ): EvolutionT[F, W, I, O, S, Unit] = for {
    env   <- EvolutionT.getEnv[F, W, I, O, S]
    state <- EvolutionT.getState[F, W, I, O, S]

    // Reset members but keep species identity
    clearedSpecies =
      state.species.map[Species[W, I, O, S]](s => s.copy(members = Vector.empty))

    // Assign genomes to existing species if possible
    (assignedSpecies, unassigned) =
      newPopulation.foldLeft((clearedSpecies, Vector.empty[Genome[W, I, O]])) {
        case ((speciesAcc, unassignedAcc), genome) =>
          speciesAcc.indexWhere { s =>
            genome.compare(s.representative, env.speciationConfig) < env.speciationConfig.threshold
          } match {
            case -1 =>
              (speciesAcc, unassignedAcc :+ genome)

            case idx =>
              val s = speciesAcc(idx)
              val updated =
                s.copy(members = s.members :+ EvaluatedGenome(genome, None, None, None))
              (speciesAcc.updated(idx, updated), unassignedAcc)
          }
      }

    clusteredNewSpecies = {
      def cluster(
        remaining: Vector[Genome[W, I, O]],
        acc: Vector[Species[W, I, O, S]],
        nextId: Long
      ): Vector[Species[W, I, O, S]] =
        remaining match {
          case Vector() => acc

          case Vector(rep, rest*) =>
            val (same, others) =
              rest.partition(g =>
                g.compare(rep, env.speciationConfig) < env.speciationConfig.threshold
              )

            val members =
              (rep +: same).map(g => EvaluatedGenome[W, I, O, S](g, None, None, None))

            val species =
              Species[W, I, O, S](
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
      _.copy(species = survivingSpecies, nextSpeciesId = state.nextSpeciesId + clusteredNewSpecies.size )
    )

  } yield ()

  def debug[F[_]: Monad: Random, W: Fractional: Order, I <: Int, O <: Int, S: Fractional: Ordering]: EvolutionT[F, W, I, O, S, Unit] = for {
    state <- EvolutionT.getState[F, W, I, O, S]
    speciesFitnesses = state.species.map { sp =>
      val fitnessSum =
        sp.members.flatMap(_.adjustedFitness).foldLeft(Fractional[S].zero)(_ + _)
      sp.id -> fitnessSum
    }.toMap
    currentGen = state.generation
    _ = {
      println(s"Generation: ${currentGen}")
      state.species.foreach { species =>
        println(s"\tSpecies: ${species.id} - age: ${species.age}, size: ${species.members.size}, score: ${speciesFitnesses(species.id)}")
      }
    }
  } yield ()

  def evolveStep[F[_]: Monad: Random, W: Fractional: Order, I <: Int, O <: Int, S: Fractional: Ordering](using RandomRange[F, W]): EvolutionT[F, W, I, O, S, Unit] = for {
    _ <- evaluateFitness
    _ <- adjustFitnessSharing
    _ <- debug
    _ <- cullSpecies
    offspringPlan <- allocateOffspringPerSpecies
    newPop <- reproduce(offspringPlan)
    _ <- speciate(newPop)
    _ <- reassignRepresentatives
    _ <- EvolutionT.modifyState[F, W, I, O, S] { state =>
      val ageIncremented = state.species.map(s => s.copy(age = s.age + 1))
      state.copy(generation = state.generation + 1, species = ageIncremented)
    }
  } yield ()

  def evolve[F[_]: Monad: Random, W: Fractional: Order, I <: Int: ValueOf, O <: Int: ValueOf, S: Fractional: Ordering](using RandomRange[F, W]): EvolutionT[F, W, I, O, S, Unit] = for {
    env <- EvolutionT.getEnv[F, W, I, O, S]
    initalPop <- EvolutionT.liftGenePool[F, W, I, O, S, Genome[W, I, O]](genome[F, W, I, O]()).replicateA(env.popsize)
    _ <- speciate(initalPop.toVector)
    _ <- List.fill(env.generations)(()).traverse_(_ => evolveStep[F, W, I, O, S])
    _ <- evaluateFitness
  } yield ()
    

  
}