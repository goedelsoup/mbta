package mbta

import cats.effect._
import cats.implicits._
import std._

import scala.concurrent.ExecutionContext.Implicits._

object App extends IOApp {

  def run(args: List[String]): IO[ExitCode] = Config.make[IO].use { config =>
    (Service.displaySubwayRoutes :: Service.displaySubwayRouteSummary :: Nil)
      .foldMapM(_.run(config))
      .flatTap(putStrLn[IO])
      .as(ExitCode.Success)
  }
}