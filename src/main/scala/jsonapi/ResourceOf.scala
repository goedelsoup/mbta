package jsonapi

import cats.ApplicativeError
import cats.implicits._
import io.circe._

/*
A traversable model describing the JSON-API specification of a resource
 */
trait ResourceOf[F[_], A] {

  /*
  The JSON-API entity
   */
  val `type`: ResourceType

  /*
  The raw entity record
   */
  val json: Json

  /*
  The decoder for the attributes blob of the JSON-API resource
   */
  def decoder: Decoder[A]

  /*
  The encoder for the attributes blob of the JSON-API resource
   */
  def encoder: Encoder[A]

  /*

   */
  def id: Decoder.Result[String] = json.hcursor
    .downField("id")
    .as[String]

  /*

   */
  def maybeType: Decoder.Result[ResourceType] = json.hcursor
    .downField("type")
    .as[String]
    .ensureOr(t => DecodingFailure(
      s"Invalid type $t, expected ${`type`.s}", Nil))(_ === `type`.s)
    .as(`type`)

  /*
  The attribute record for the JSON-API resource
   */
  def attributes: Decoder.Result[A] = json.hcursor
    .downField("attributes")
    .focus
    .fold(DecodingFailure("No attributes object found", Nil)
      .asLeft[A])(decoder.decodeJson)


  /*
  Validates the instance against the structural and named spec
   */
  def validated(implicit
                A: ApplicativeError[F, Throwable])
  : Decoder.Result[ResourceOf[F, A]] =
    (id *> maybeType *> attributes).as(this)
}

object ResourceOf {

  implicit def resourceOfDecoder[F[_], A](implicit
                                          R: ResourceOf[F, A],
                                          A: ApplicativeError[F, Throwable])
  : Decoder[ResourceOf[F, A]] = Decoder.instance(_ => R.validated)
}