package notionfys

import cats.data.Kleisli
import cats.effect.IO

object App {
  val AppM = Kleisli
  type AppM[A] = Kleisli[IO, Args, A]
}
