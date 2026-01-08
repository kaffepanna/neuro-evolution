package se.randomserver.ne

import cats.syntax.all.*
import cats.effect.std.Dispatcher
import cats.effect.IO
import cats.effect.FiberIO

class BackgroundRuntime(dispatcher: Dispatcher[IO], computeThreads: Int) {
  private var fiber: Option[FiberIO[Unit]] = None

  def start(program: IO[Unit]): Unit = dispatcher.unsafeRunAndForget {
    program.start.flatMap { f => IO { fiber = Some(f)} }
  }

  def stop(): Unit = dispatcher.unsafeRunSync(stopIO())

  def stopIO(): IO[Unit] = fiber.fold(IO.unit) { f =>
    f.cancel 
  }

  def shutdown(): IO[Unit] =
    stopIO() >> IO {
      javafx.application.Platform.exit()
    }
}
