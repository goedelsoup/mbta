package mbta.api.domain

import cats.Show
import cats.effect._
import cats.implicits._
import eu.timepit.refined.types.string.HexString
import io.circe._
import io.circe.refined._
import io.circe.syntax._
import jsonapi._

case class Route(color: HexString,
                 description: String,
                 directionDestinations: List[String],
                 directionNames: List[String],
                 fareClass: String,
                 longName: String,
                 shortName: String,
                 sortOrder: Int,
                 textColor: HexString,
                 `type`: RouteClass)

object Route {

  def of[F[_]: Sync](j: Json): ResourceOf[F, Route] =
    new ResourceOf[F, Route] {

      val `type`: ResourceType = types.Route

      val json: Json = j

      def encoder: Encoder[Route] = Encoder.instance { route =>
        Json.obj(
          "color" -> route.color.asJson,
          "description" -> route.description.asJson,
          "direction_destinations" -> route.directionDestinations.asJson,
          "direction_names" -> route.directionNames.asJson,
          "fare_class" -> route.fareClass.asJson,
          "long_name" -> route.longName.asJson,
          "short_name" -> route.shortName.asJson,
          "sort_order" -> route.sortOrder.asJson,
          "text_color" -> route.textColor.asJson,
          "type" -> route.`type`.id.asJson
        )
      }

      def decoder: Decoder[Route] = Decoder
        .forProduct10(
          "color",
          "description",
          "direction_destinations",
          "direction_names",
          "fare_class",
          "long_name",
          "short_name",
          "sort_order",
          "text_color",
          "type")(Route.apply)
    }

  implicit val routeShow: Show[Route] =
    (r0: Route) => r0.longName
}
