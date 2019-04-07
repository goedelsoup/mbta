package mbta.api

import cats.{ApplicativeError, FlatMap}
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.implicits._
import io.chrisdavenport.mules.MemoryCache
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

  type RouteName = String

  type Routes[F[_]] = NonEmptyList[ResourceOf[F, Route]]
  type Stops[F[_]] = NonEmptyList[ResourceOf[F, Stop]]

  /*
  An http4s implementation of the MBTA algebra

  See MAlg for documentation; only minimal http machinery should be performed here
   */
  def apply[F[_]: Sync](C: Client[F],
                        R: MemoryCache[F, Seq[RouteClass], Routes[F]],
                        S: MemoryCache[F, RouteName, Stops[F]])
  : ApiAlg[F] =
    new ApiAlg[F] with Http4sDsl[F] {

      /*
      Get Routes by class
       */
      def routesOf(rs: RouteClass*): ResourceCollectionF[Route] =
        R.lookup(rs) >>= { maybeRoutes => // Check if the query is in the cache, otherwise call the API
          // todo how to override default query param monoid behavior; this API uses CSV rather than append &s
          val routeClassString =
            rs.toList.map(_.id.show).intercalate(",")
          val getRoutes =
            extractResources[F, Route](Route.of)(C.expect[Json](Request[F](
              method = GET,
              uri = root / "routes" +? ("filter[type]", routeClassString)
            )))
          maybeRoutes.fold(getRoutes
            .flatTap(r => R.insert(rs, r)))(_.pure[F])
        }


      /*
      Get Stops by RouteName
       */
      def stopsFor(rs: String): ResourceCollectionF[Stop] =
        S.lookup(rs) >>= { maybeStops => // Check if the query is in the cache, otherwise call the API
          val getStops =
            extractResources[F, Stop](Stop.of)(C.expect[Json](Request[F](
              method = GET,
              uri = root / "stops" +? ("filter[route]", rs)
            )))
          maybeStops.fold(getStops
            .flatTap(s => S.insert(rs, s)))(_.pure[F])
        }
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
