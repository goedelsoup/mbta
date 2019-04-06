package mbta.api.domain

import cats.Eq
import cats.implicits._
import jsonapi.ResourceType

sealed abstract class MBTAType(val _s: String)
  extends ResourceType(_s)

object MBTAType {

  implicit val mbtaTypeEq: Eq[MBTAType] =
    (m0: MBTAType, m1: MBTAType) => m0.s === m1.s
}

object types {
  case object Route extends MBTAType("route")
  case object Stop  extends MBTAType("stop")
}
