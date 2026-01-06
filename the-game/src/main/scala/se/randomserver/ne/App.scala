package se.randomserver.ne

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.std.Dispatcher

object App extends IOApp.Simple {
  override def run: IO[Unit] = {
    Dispatcher.parallel[IO].use { dispatcher =>
      IO {
        val runtime = BackgroundRuntime(dispatcher)
        val uiApp = GridVisualizer(dispatcher, runtime)

        Runtime.getRuntime.addShutdownHook(
          new Thread(() =>
            dispatcher.unsafeRunAndForget(runtime.shutdown())
          )
        )

        uiApp.main(Array.empty)
      }
    }
  }
}
