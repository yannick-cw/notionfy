package notionfys

import App._
import cats.effect.IO

trait Console[F[_]] {
  def verboseLog(msg: String): F[Unit]
  def log(msg: String): F[Unit]
}

object Console extends Console[AppM] {

  override def log(msg: String): App.AppM[Unit] = AppM.liftF(IO(println(msg)))

  override def verboseLog(msg: String): AppM[Unit] =
    AppM
      .ask[IO, Args]
      .flatMap(args => if (args.verbose) AppM.liftF(IO(println(msg))) else AppM.pure(()))
}
