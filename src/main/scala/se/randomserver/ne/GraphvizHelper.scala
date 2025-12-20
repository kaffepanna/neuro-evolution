package se.randomserver.ne

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

object GraphvizHelper:

  def graphGenome(g: Genome[Double, 2, 1]): GraphViz.Grammar[Unit] = {
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

    def plotGenome[F[_]: Processes: Concurrent](g: Genome[Double, 2, 1]): F[String] = {
      val graph = graphGenome(g).foldMap(StringCompiler.compiler()).run._1
      (for {
          dot <- ProcessBuilder("dot", "-Tpng").spawn[F]
          feh <- ProcessBuilder("feh", "-").withExtraEnv(Map("DISPLAY" -> ":0")).spawn[F]
      } yield feh -> dot).use {case (feh, dot) =>
          val in = Stream.emit(graph + "\n")
                      .debug(i => s"Graphviz input $i")
                      .through(text.utf8.encode)
                      .through(dot.stdin)
          val pipe = dot.stdout
              .through(feh.stdin)
              .concurrently(in)

          for {
              a <- pipe.compile.string
              _ = println(s"feh output: $a")
              fehExit <- feh.exitValue
              _ = println(s"Feh exited with $fehExit")
              dotExit <- dot.exitValue
              _ = println(s"Dot exited with $dotExit")
          } yield a
      }
    }
end GraphvizHelper
