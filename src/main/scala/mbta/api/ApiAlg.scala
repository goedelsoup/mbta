package mbta.api

import cats.data.NonEmptyList
import jsonapi.ResourceOf
import mbta.api.domain._

/*

 */
trait ApiAlg[F[_]] {

  type IdentifiedResource[A] = F[ResourceOf[F, A]]

  type ResourceCollectionF[A] = F[NonEmptyList[ResourceOf[F, A]]]

  /*

   */
  def routesOf(rs: RouteClass*): ResourceCollectionF[Route]

  /*

   */
  def stopsFor(rs: String*): ResourceCollectionF[Stop]
}
