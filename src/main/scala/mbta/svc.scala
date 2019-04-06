package mbta

import cats.{Order, Show}
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.IO
import cats.implicits._
import jsonapi.ResourceOf
import mbta.api.domain._
import mbta.std._

import scala.collection.SortedMap

object svc {

  type ServiceK[A] = Kleisli[IO, Config[IO], A]

  type RouteResource = ResourceOf[IO, Route]
  type StopResource = ResourceOf[IO, Stop]

  type Pair = (RouteResource, NonEmptyList[StopResource])
  type FlatPair = (RouteResource, StopResource)

  /*
  Step 1 - Display all long names of routes which we have defined a subway routes

  Where:
    - A subway route is a LightRail(0) or HeavyRail(1) class entity
    - A route shall be referred to by its long name through out the user session

  The filter query parameters should always be used based on our requirements as we should
  be good citizens in a distributed micro-service architecture. That means utilizing
  push-down filters everywhere possible. Here we dictate that we shall only ever care about
  the routes we classify as subways and, as such, should only ask the remote API for those
  minimizing both the network payload required for the additional records but removing the
  need for us to perform an additional filter step on the client-side.
   */
  def displaySubwayRoutes: ServiceK[String] =
    Kleisli { implicit service =>
      for {
        routes <- service.api.routesOf(LightRail, HeavyRail)
        routes <- routes.traverse(_.attributes.liftTo[IO])
        result  = routes.mkString_("===\nAnswer (1)\n===\n", "\n", "\n===")
      } yield result
    }

  /*
  Step 2-a - Display the max/min routes based on stop count

  Since our specification uses the definite singular to describe the expected
  output, we would model our return type to be the `RouteName`. Then, by describing
  a total order on the size of the co-domain from `RouteName` to `Set[Stop]`, we can
  attain the minimum and maximum results according to our requirements.

  However, it should become apparent when thinking through the requirements as such,
  they actually inadequately describe the domain because nothing in reality prohibits
  two routes from having the same number of stops. In fact, when we implement generative
  property testing on such a model, it will identify the fallacy quite quickly.

  Thus, we must modify our original approach, using a partial-order instead and modifying our
  return type to be `Nel[RouteName]`, reflecting the certainty that there will be at
  least one result, but the indeterminacy of whether it shall only have one result.

  Step 2-b - Display a report of all stops which connect two or more routes

   */
  def displaySubwayRouteSummary: ServiceK[String] =
    Kleisli { implicit service =>

      for {
        routes   <- service.api.routesOf(LightRail, HeavyRail)
        stopMap  <- routes.traverse(getStops)

        max       = stopMap.maximum(orderForStopCount)
        maxId    <- max._1.id.liftTo[IO]
        maxSize   = max._2.size

        min       = stopMap.minimum(orderForStopCount)
        minId    <- min._1.id.liftTo[IO]
        minSize   = min._2.size

        routeMap  = groupRoutesForStopsWhere(_.size >= 2)(stopMap)

        result    =
          show"""
             |***
             |Answer (2)
             |***
             |   Most stops: $maxId ($maxSize)
             | Fewest stops: $minId ($minSize)
             |
             |Stops with more than 2 routes:
             |${routeQueryResultShow.show(routeMap)}
             |***
           """.stripMargin
      } yield result
    }

  /*
  The API only allows you to include stops if you are filtering them, so
  we need to traverse each result.
   */
  def getStops(route: RouteResource)
              (implicit service: Config[IO]): IO[Pair] =
    for {
      id <- route.id.liftTo[IO]
      stops <- service.api.stopsFor(id)
    } yield route -> stops

  /*
  Return a map of stops to routes which match a predicate on either record
   */
  def groupRoutesForStopsWhere(fa: NonEmptyList[FlatPair] => Boolean)
                              (sm: NonEmptyList[Pair]): SortedMap[Option[String], List[String]] = sm
    .flatMap(flattenPairs)
    .groupByNem(_._2.attributes.map(_.name).toOption)
    .filter(fa)
    .mapValues(toRouteNames)

  /*
  Flatten a Route => Nel[Stop] map so that we can pivot it on stop information
   */
  def flattenPairs(p: Pair): NonEmptyList[FlatPair] = p._2.map(s => p._1 -> s)

  /*
  Extract the route's long name from a record
   */
  def toRouteNames(rs: NonEmptyList[FlatPair]): List[String] = rs
    .map(_._1.attributes
      .map(_.longName)
      .toOption
      .get) // todo fix
    .toList

  /*
  2-(a/b) the route with the most and least stops can be established as a
  total ordering of the map pairs from Route -> Nel[Stop]
   */
  implicit val orderForStopCount: Order[Pair] =
    (p0: Pair, p1: Pair) => Order[Int].compare(p0._2.size, p1._2.size)

  /*
  Print a route map summarizing the stops it contains
   */
  implicit val routeQueryResultShow: Show[SortedMap[Option[String], List[String]]] =
    sm => sm.map { p =>
      val stop = p._1.getOrElse("<!> UNK")
      val routes = p._2.mkString_("[", "|", "]")
      show"$stop => $routes"
    }
    .toList
    .mkString_("\n", "\n", "\n")
}
