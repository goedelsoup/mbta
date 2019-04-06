package mbta.api.domain

import jsonapi.ResourceType

sealed abstract class MBTAType(val _s: String)
  extends ResourceType(_s)

object types {
  case object Route extends MBTAType("route")
  case object Stop  extends MBTAType("stop")
}
