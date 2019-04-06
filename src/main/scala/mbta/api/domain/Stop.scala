package mbta.api.domain

import cats.effect.Sync
import io.circe._
import io.circe.generic.semiauto._
import jsonapi.{ResourceOf, ResourceType}

case class Stop(name: String,
                description: Option[String],
                address: Option[String])

object Stop {

  def of[F[_]: Sync](j: Json): ResourceOf[F, Stop] =
    new ResourceOf[F, Stop] {

      val `type`: ResourceType = types.Stop

      val json: Json = j

      def encoder: Encoder[Stop] = deriveEncoder

      def decoder: Decoder[Stop] = deriveDecoder
    }
}
