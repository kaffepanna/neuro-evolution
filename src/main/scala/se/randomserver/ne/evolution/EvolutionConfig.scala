package se.randomserver.ne.evolution

import pureconfig.*
import pureconfig.generic.derivation.default.*

final case class EvolutionConfig(
  populationSize: Int,
  generations: Int,

  eliteFraction: Double,

  weightChance: Double,
  resetChance: Double,
  connectionChance: Double,
  nodeChance: Double,

  defaultBias: Double,
  targetFitness: Option[Double],
  recurrentSteps: Int
) derives ConfigReader