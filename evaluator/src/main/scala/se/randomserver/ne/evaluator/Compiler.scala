package se.randomserver.ne.evaluator

import se.randomserver.ne.genome.Genome
import se.randomserver.ne.Graph

object Compiler {
  
  type SCC[V] = Set[V]
  type BlockId = Int

  final case class Block(
    id: BlockId,
    nodes: Set[Int]
  )


  final case class BlockGraph(
    blocks: Vector[Block],              // nodes per block
    edges: Map[BlockId, Set[BlockId]]       // block -> block
  )

  
  final case class CompiledConnection[W](
    from: Int,
    weight: W
  )

  final case class CompiledNode[W](
    id: Int,
    incoming: Vector[CompiledConnection[W]]
  )

  final case class CompiledBlock[W](
    nodes: Vector[CompiledNode[W]],     // internal order arbitrary
    isRecurrent: Boolean                // nodes.size > 1 OR self-loop
  )

  final case class CompiledNetwork[W](
    blocks: Vector[CompiledBlock[W]],   // topo-sorted blocks
    inputs: Set[Int],
    bias: Set[Int],
    outputs: Set[Int],
    transfer: W => W
  )

  def compileGenomeToBlockGraph(genome: Genome[?, ?, ?]): BlockGraph = {
    val edges: Map[Int, Set[Int]] =
      genome.genes.toVector
        .filter(_.enabled)
        .groupMap(_.from)(_.to)
        .view.mapValues(_.toSet).toMap
        .withDefaultValue(Set.empty)

    val sccs: Vector[SCC[Int]] = Graph.tarjanSCC(genome.nodes.toSet, edges.apply).toVector
    val nodeToBlock: Map[Int, BlockId] = sccs.zipWithIndex.flatMap { case (scc, id) =>
      scc.map(_ -> id)
    }.toMap

    val blockEdges: Map[BlockId, Set[BlockId]] =
      genome.genes
        .filter(_.enabled)
        .foldLeft(Map.empty[BlockId, Set[BlockId]].withDefaultValue(Set.empty)) {
          case (acc, g) =>
            val fromB = nodeToBlock(g.from)
            val toB   = nodeToBlock(g.to)
            if (fromB == toB) acc
            else acc.updated(fromB, acc(fromB) + toB)
        }

    val sortedBlockIds: Seq[BlockId] = Graph.kahnTopoSort(nodeToBlock.values.toSet, blockEdges.apply)
    
    BlockGraph(
      sortedBlockIds.toVector.map { id =>
        Block(id, sccs(id))
      },
      blockEdges
    )
  }

  def compileGenome[W](genome: Genome[W, ?, ?], transfer: W => W): CompiledNetwork[W] = {
    // sorted block graph
    val blockGraph: BlockGraph = compileGenomeToBlockGraph(genome)
    val enabledGenes = genome.genes.filter(_.enabled)

    val incomingByNode: Map[Int, Vector[CompiledConnection[W]]] =
      enabledGenes
        .groupBy(_.to)
        .view
        .mapValues(_.map(g => CompiledConnection(g.from, g.weight)).toVector)
        .toMap
        .withDefaultValue(Vector.empty)

    def compileBlock(block: Block): CompiledBlock[W] = {
      val nodes =
        block.nodes.toVector.map { id =>
          CompiledNode(
            id = id,
            incoming = incomingByNode(id)
          )
        }

      val isRecurrent =
        block.nodes.size > 1 ||
        enabledGenes.exists(g => g.from == g.to && block.nodes.contains(g.from))

      CompiledBlock(
        nodes = nodes,
        isRecurrent = isRecurrent
      )
    }

    val compiledBlocks =
      blockGraph.blocks.map { block =>
        compileBlock(block)
      }

    CompiledNetwork(
      blocks  = compiledBlocks,
      inputs  = genome.inputs.toSet,
      bias    = genome.bias.toSet,
      outputs = genome.outputs.toSet,
      transfer = transfer
    )
  }
}

