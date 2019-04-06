package mbta

import cats.Order
import cats.data.{Kleisli, NonEmptyList}
import cats.effect.IO
import cats.implicits._
import jsonapi.ResourceOf
import mbta.api.domain._
import mbta.std._

object svc {

  type ServiceK[A] = Kleisli[IO, Config[IO], A]

  type RouteResource = ResourceOf[IO, Route]
  type StopResource = ResourceOf[IO, Stop]

  def displaySubwayRoutes: ServiceK[String] =
    Kleisli { implicit service =>
      for {
        routes <- service.api.routesOf(LightRail, HeavyRail)
        routes <- routes.traverse(_.attributes.liftTo[IO])
        result  = routes.mkString_("===\nAnswer (1)\n===\n", "\n", "\n===")
      } yield result
    }

  def displaySubwayRouteSummary: ServiceK[String] =
    Kleisli { implicit service =>

      type Pair = (RouteResource, NonEmptyList[StopResource])
      type FlatPair = (RouteResource, StopResource)

      /*
      2-(a/b) the route with the most and least stops can be established as a
      total ordering of the map pairs from Route -> Nel[Stop]
       */
      implicit def orderForStopCount: Order[Pair] =
        (p0: Pair, p1: Pair) => Order[Int].compare(p0._2.size, p1._2.size)

      /*
      The API only allows you to include stops if you are filtering them, so
      we need to traverse each result.

      todo see if this can be chunked for perf
       */
      def getStops(route: RouteResource): IO[Pair] =
        for {
          id <- route.id.liftTo[IO]
          stops <- service.api.stopsFor(id)
        } yield route -> stops

      def flattenPairs(p: Pair): NonEmptyList[FlatPair] = p._2.map(s => p._1 -> s)

      def pluckRouteIds(rs: NonEmptyList[FlatPair]): List[String] = rs.map(_._1.id.toOption.get).toList

      for {
        routes   <- service.api.routesOf(LightRail, HeavyRail)
        stopMap  <- routes.traverse(getStops)

        max       = stopMap.maximum(orderForStopCount)
        maxId    <- max._1.id.liftTo[IO]
        maxSize   = max._2.size

        min       = stopMap.minimum(orderForStopCount)
        minId    <- min._1.id.liftTo[IO]
        minSize   = min._2.size

        routeMap  = stopMap
          .flatMap(flattenPairs)
          .groupByNem(_._2.id.toOption.get) // todo this is naughty
          .filter(rs => rs.size >= 2)
          .mapValues(pluckRouteIds)
          .map(p =>
            s"${p._1} => ${p._2
              .mkString_("[", "|", "]")}" // todo show
          )
          .toList
          .mkString_("\n", "\n", "\n") // todo show

        result    =
          show"""
             |***
             |Answer (2)
             |***
             |   Most stops: $maxId ($maxSize)
             | Fewest stops: $minId ($minSize)
             |
             |Stops with more than 2 routes:
             |$routeMap
             |***
           """.stripMargin
      } yield result
    }
//
//  def displayRouteBetween(origin: String, destination: String): ServiceK[Unit] =
//    Kleisli { implicit service =>
//      for {
//        route <- service.api.routeFor(origin, destination)
//        route <- route.attributes.liftTo[IO]
//        _     <- putStrLn[IO, Route](route)
//      } yield ()
//    }
}
