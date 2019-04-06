package mbta

import cats.Show
import cats.effect.{IO, Sync}
import cats.implicits._

object std {

  def readEnv[F[_]: Sync](s: String): F[String] = sys.env.get(s)
    .liftTo[F](new RuntimeException(s"Config value $s not found"))

  def putStrLn[F[_]: Sync](s: String): F[Unit] = Sync[F].delay(println(s))

  def putStrLn[F[_]: Sync, A: Show](a: A): F[Unit] = Sync[F].delay(println(a.show))
}
