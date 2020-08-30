package notionfys

import cats.data.Kleisli
import cats.effect.IO
import sttp.model.StatusCode

object App {
  val AppM = Kleisli
  type AppM[A] = Kleisli[IO, Args, A]

  sealed trait Error extends Throwable

  case class FileNotFoundErr(path: os.Path)                    extends Error
  case class HttpResponseError(code: StatusCode, body: String) extends Error
  case class DecodingError(errMsg: String, body: String)       extends Error

}
