package se.randomserver.ne

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.{*, given}
import se.randomserver.ne.process.Process
object TestMain extends IOApp{
  override def run(args: List[String]): IO[ExitCode] =
    Process.spawn[IO]("ls", "-l").use { p =>
      List.fill(6)(()).map (_ => p.stdOut.take).sequence >>= { a =>
        IO.println(a)
      }

    }.map(_ => ExitCode.Success)
}
