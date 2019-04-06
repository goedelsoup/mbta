package mbta.api

import cats.{ApplicativeError, FlatMap}
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import io.circe.Json
import jsonapi.ResourceOf
import mbta.api.domain._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.Uri.uri
import org.http4s.client.Client

object v3 {

  private[mbta] val root: Uri = uri("https://api-v3.mbta.com")

  /*
  An http4s implementation of the MBTA algebra

  See MAlg for documentation; only minimal http machinery should be performed here
   */
  def apply[F[_]: Sync](C: Client[F]): ApiAlg[F] =
    new ApiAlg[F] with Http4sDsl[F] {

      def routes: ResourceCollectionF[Route] =
        extractResources[F, Route](Route.of)(C.expect[Json](Request[F](
          method = GET,
          uri = root / "routes"
        )))

      def routesOf(rs: RouteClass*): ResourceCollectionF[Route] =
        extractResources[F, Route](Route.of)(C.expect[Json](Request[F](
          method = GET,
          uri = root / "routes" +? ("filter[type]", rs.toList.map(_.id.show).intercalate(","))
        )))

      def stopsFor(rs: String*): ResourceCollectionF[Stop] =
        extractResources[F, Stop](Stop.of)(C.expect[Json](Request[F](
          method = GET,
          uri = root / "stops" +? ("filter[route]", rs.toList.intercalate(","))
        )))
    }

  /*
  Tap into the `data` field of the JSON-API record and retrieve
  a non-empty list of resources.
   */
  def extractResources[F[_], A](fa: Json => ResourceOf[F, A])
                               (result: F[Json])
                               (implicit
                                F: FlatMap[F],
                                A: ApplicativeError[F, Throwable])
  : F[NonEmptyList[ResourceOf[F, A]]] =
    result >>= (_.hcursor
      .downField("data")
      .as[NonEmptyList[Json]]
      .flatMap(_.traverse(fa(_).validated))
      .liftTo[F])
}
