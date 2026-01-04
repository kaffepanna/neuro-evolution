package se.randomserver.ne.graphviz

import cats.syntax.all.*
import se.randomserver.ne.genome.Genome
import se.randomserver.ne.graphviz.GraphViz
import cats.syntax.all.{*, given}
import fs2.io.process.{Process, ProcessBuilder}
import se.randomserver.ne.graphviz.StringCompiler
import cats.effect.Async
import cats.effect.IO
import fs2.io.process.Processes
import fs2.{*, given}
import cats.effect.Concurrent
import se.randomserver.ne.evaluator.Compiler.CompiledNetwork
import se.randomserver.ne.evaluator.Compiler.CompiledBlock
import se.randomserver.ne.evaluator.Compiler.CompiledNode
import cats.free.Free

object GraphvizHelper:
  import GraphViz._
  def graphBlock(net: CompiledNetwork[Double])(block: CompiledBlock[Double]): GraphViz.Grammar[Unit] = {
    val blockId = net.blocks.indexOf(block)
    subgraph(s"cluster_block_${blockId}") {
      //attribute("newrank", "true") *>
      //attribute("rankdir", "LR") *>
      //attribute("rank", "same") *>
      label(s"Block: ${blockId}") *>
      block.nodes.traverse_ {n => node(n.id.toString) }
    }
  }

  def graphCompiled(net: CompiledNetwork[Double]): GraphViz.Grammar[Unit] = {
    import GraphViz._
    val inputs = net.blocks.filter(b => b.nodes.forall(n => net.inputs.contains(n.id) || net.bias.contains(n.id)))
    val outputs = net.blocks.filter(b => b.nodes.forall(n => net.outputs.contains(n.id)))
    val rest = net.blocks.filterNot(b => (inputs ++ outputs).contains(b))
    digraph {
      label("Graph") *>
      attribute("splines", "true") *>
      //attribute("rankdir", "TB")  *>
      subgraph("cluster_inputs") {
        attribute("newrank", "true") *>
        attribute("rank", "same") *>
        attribute("rankdir", "LR") *>
        label("inputs") *>
        inputs.traverse_ { blk =>
          blk.nodes.traverse_ { n => 
            node(n.id.toString)
          }
        }
      } *>
      subgraph("cluster_hidden") {
        attribute("newrank", "true") *>
        attribute("compound", "true") *>
        label("hidden") *>
        rest.traverse_(graphBlock(net))
      } *>
      subgraph("cluster_outputs") {
        attribute("newrank", "true") *>
        //attribute("rank", "same") *>
        //attribute("rankdir", "LR")
        label("Outputs") *>
        outputs.traverse_ { blk =>
          blk.nodes.traverse_ { n => 
            node(n.id.toString)
          }
        }
      } *>
      net.blocks.traverse_ { block =>
        block.nodes.traverse_ { node =>
          node.incoming.traverse_ { incoming =>
            edge_(incoming.from.toString, node.id.toString)
          }  
        }
      }
    }
  }

  def graphGenome(g: Genome[Double]): GraphViz.Grammar[Unit] = {
    import GraphViz._
    digraph {
      for {
        _ <- attribute("rankdir" ,"LR")
        inputs <- subgraph("cluster_inputs")(for {
          _ <- label("inputs")
          _ <- g.inputs.toList.map(nid => node(nid.toString)).sequence
        } yield ())
  
        hidden <- subgraph("cluster_hidden") {
          g.hidden.toList.map(nid => node(nid.toString)).sequence
        }

        outputs <- subgraph("cluster_output")(for {
          _ <- label("outputs")
          _ <- g.outputs.toList.map(nid => node(nid.toString)).sequence
        } yield ())
 
        bias <- subgraph("cluster_bias")(for {
          _ <- label("bias")
          _ <- g.bias.toList.map(nid => node(nid.toString)).sequence
        } yield ())
    
        edges <- g.genes
          .filter(gene => gene.enabled)
          .toList
          .map { gene =>
            edge_(gene.from.toString, gene.to.toString, "label" -> s"w: ${String.format("%f", gene.weight)}")
          }.sequence
      } yield ()
    }
  }

    def plotCompiled[F[_]: Processes: Concurrent](g: CompiledNetwork[Double]): F[String] = {
      val graph = graphCompiled(g).foldMap(StringCompiler.compiler()).run._1
      (for {
          dot <- ProcessBuilder("dot", "-Tpng").spawn[F]
          feh <- ProcessBuilder("feh", "-").withExtraEnv(Map("DISPLAY" -> ":0")).spawn[F]
      } yield feh -> dot).use {case (feh, dot) =>
          val in = Stream.emit(graph + "\n").debug()
                      .through(text.utf8.encode)
                      .through(dot.stdin)
          val pipe = dot.stdout
              .through(feh.stdin)
              .concurrently(in)

          for {
              a <- pipe.compile.string
              fehExit <- feh.exitValue
              dotExit <- dot.exitValue
          } yield a
      }
    }
end GraphvizHelper
