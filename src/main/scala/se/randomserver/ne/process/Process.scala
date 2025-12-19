package se.randomserver.ne.process

import cats.effect.std.{Dispatcher, Queue}
import cats.syntax.all.{*, given}
import cats.effect.{Async, Deferred, Resource}

import java.io.{BufferedInputStream, BufferedReader, InputStreamReader, StringReader}
import scala.io.Source

trait Process[F[_]] {
  val stdOut: Queue[F, String]

  def kill(): F[Unit]
}

object Process:
  import scala.sys.process.{Process => ScalaProcess, ProcessIO}
  def spawn[F[_]: Async](command: String, args: String*): Resource[F, Process[F]] = Dispatcher[F].flatMap { dispatcher =>
    val proc = for {
      dProcess <- Deferred[F, Process[F]]
      stdout <- Queue.unbounded[F, String]
      stdin <- Queue.unbounded[F, String]

      processControl = new ProcessIO(
        writer =>  (),
        input => {
          val bufferd = new BufferedReader(new InputStreamReader(input))
          while(bufferd.ready()) {
            val line = bufferd.readLine()
            dispatcher.unsafeRunSync(stdout.offer(line))
          }
        },
        _ => ()
      )

      builder = ScalaProcess(command)
      fiber <- Async[F].start(Async[F].delay(builder.run(processControl)))
      _ <- dProcess.complete(new Process[F] {
            val stdOut: Queue[F, String] = stdout
            def kill(): F[Unit] = fiber.cancel
      })

      process <- dProcess.get

    } yield process

    Resource.make(proc)(p => p.kill())
  }
end Process
