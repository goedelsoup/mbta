package mbta

import cats.{ApplicativeError, Show}
import cats.effect.{IO, Sync}
import cats.implicits._

object std {

  /*
  Read an environment variable into a monadic context
   */
  def readEnv[F[_]](s: String)
                   (implicit A: ApplicativeError[F, Throwable]): F[String] = sys.env.get(s)
    .liftTo[F](new RuntimeException(s"Config value $s not found"))

  /*
  Print a string in a monadic context
   */
  def putStrLn[F[_]: Sync](s: String): F[Unit] = Sync[F].delay(println(s))

  /*
  Print a Show-able record in a monadic context
   */
  def putStrLn[F[_]: Sync, A: Show](a: A): F[Unit] = Sync[F].delay(println(a.show))
}
