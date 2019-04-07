package mbta

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.effect._
import cats.implicits._
import io.chrisdavenport.mules._
import jsonapi.ResourceOf
import mbta.api._
import mbta.api.domain.{Route, RouteClass, Stop}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.middleware.Logger

import scala.concurrent.ExecutionContext

case class Config[F[_]](api: ApiAlg[F])

object Config {

  /*
  Construct all of our core application dependencies, wrapped in Resource so we ensure
  all things are properly cleaned up.
   */
  def make[F[_]](implicit
                 C: ConcurrentEffect[F],
                 T: Timer[F],
                 E: ExecutionContext)
  : Resource[F, Config[F]] = for {

    routeCache <- Resource.liftF(MemoryCache
      .createMemoryCache[F, Seq[RouteClass], NonEmptyList[ResourceOf[F, Route]]](None))

    stopCache  <- Resource.liftF(MemoryCache
      .createMemoryCache[F, String, NonEmptyList[ResourceOf[F, Stop]]](None))

    logCalls  <- Resource
                   .liftF(std.readEnv[F]("LOG_CALLS")
                     .recover { case _ => "false" })
    logCalls  <- Resource.liftF(maybeBoolean[F](logCalls))
    client    <- BlazeClientBuilder[F](E)
                   .resource
                   .map(Logger[F](logHeaders = logCalls, logBody = logCalls))
  } yield
    Config[F](v3(client, routeCache, stopCache))

  /*
  Lifts a string into a F[Boolean], raising an exception on invalid strings
   */
  private def maybeBoolean[F[_]](s: String)
                                (implicit A: ApplicativeError[F, Throwable])
  : F[Boolean] = s match {
    case "true"  => A.pure(true)
    case "false" => A.pure(false)
    case _       => A.raiseError[Boolean](
                      new RuntimeException(s"Invalid boolean expression: $s"))
  }
}
