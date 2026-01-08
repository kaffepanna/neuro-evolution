package se.randomserver.ne

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.std.Dispatcher
import scala.concurrent.ExecutionContext
import cats.effect.unsafe.IORuntime
import java.util.concurrent.Executors

object App {

  val computeThreads = math.max(1, Runtime.getRuntime().availableProcessors()*2 - 1)

  println(s"Compue threads allocated $computeThreads")

  given computeEC: ExecutionContext =
    ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(computeThreads)
    )

  given runtime: IORuntime =
    IORuntime.builder()
      .setCompute(computeEC, () => ())
      .build()

  def main(args: Array[String]): Unit = {
    given ExecutionContext = computeEC

    Dispatcher.parallel[IO].use { dispatcher =>
      IO {
        val runtime = BackgroundRuntime(dispatcher, computeThreads)
        val uiApp = GridVisualizer(dispatcher, runtime)

        Runtime.getRuntime.addShutdownHook(
          new Thread(() =>
            dispatcher.unsafeRunAndForget(runtime.shutdown())
          )
        )

        uiApp.main(args)
      }
    }.unsafeRunSync()
  }
}
