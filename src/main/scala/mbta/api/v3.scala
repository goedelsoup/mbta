package mbta.api

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

      /*

       */
      def routes: ResourceCollectionF[Route] =
        C.expect[Json](Request[F](
          method = GET,
          uri = root / "routes"
        ))
        .flatMap(_.hcursor
          .downField("data")
          .as[NonEmptyList[Json]]
          .flatMap(_.traverse(Route.of(_).validated))
          .liftTo[F])

      /*

       */
      def routesOf(rs: RouteClass*): ResourceCollectionF[Route] =
        C.expect[Json](Request[F](
          method = GET,
          uri = root / "routes" +? ("filter[type]", rs.toList.map(_.id.show).intercalate(","))
        ))
        .flatMap(_.hcursor
          .downField("data")
          .as[NonEmptyList[Json]]
          .flatMap(_.traverse(Route.of(_).validated))
          .liftTo[F])

      def stopsFor(rs: String*): ResourceCollectionF[Stop] =
        C.expect[Json](Request[F](
          method = GET,
          uri = root / "stops" +? ("filter[route]", rs.toList.intercalate(","))
        ))
        .flatMap(_.hcursor
          .downField("data")
          .as[NonEmptyList[Json]]
          .flatMap(_.traverse(Stop.of(_).validated))
          .liftTo[F])
    }
}
