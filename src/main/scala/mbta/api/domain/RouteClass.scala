package mbta.api.domain

import cats._
import cats.implicits._
import io.circe._
import io.circe.syntax._

/*
The high-level mode of transportation to which a route belongs
 */
sealed abstract class RouteClass(val id: Int)

case object LightRail extends RouteClass(0)
case object HeavyRail extends RouteClass(1)
case object CommuterRail extends RouteClass(2)
case object Bus extends RouteClass(3)
case object Ferry extends RouteClass(4)

object RouteClass {

  /*
  Non-total constructor as classes could be added out-of-band with these sources
   */
  def apply(i: Int): Option[RouteClass] = i match {
    case 0 => LightRail.some
    case 1 => HeavyRail.some
    case 2 => CommuterRail.some
    case 3 => Bus.some
    case 4 => Ferry.some
    case _ => None
  }

  /*
  Non-universal identity based on underlying ids

  This must be subjected to generative property tests to ensure we have lawful uniqueness, which cannot
  be assessed by the compiler.
   */
  implicit val routeClassEq: Eq[RouteClass] =
    (x: RouteClass, y: RouteClass) => x.id === y.id

  /*
  Human friendly string; not guaranteed to be isomorphic
   */
  implicit val routeClassShow: Show[RouteClass] = {
    case LightRail => "Light Rail"
    case HeavyRail => "Heavy Rail"
    case CommuterRail => "Commuter Rail"
    case Bus => "Bus"
    case Ferry => "Ferry"
  }

  implicit val routeClassDecoder: Decoder[RouteClass] = Decoder.instance { cursor =>
    cursor.as[Int].flatMap(RouteClass(_).toRight(DecodingFailure("Invalid ID", Nil)))
  }

  implicit val routeClassEncoder: Encoder[RouteClass] = Encoder.instance(_.id.asJson)
}
