package notionfys

import App._
import cats.effect.IO
import cats.implicits.{catsSyntaxApply, catsSyntaxIfM}

trait FS[F[_]] {
  def readF(p: os.Path): F[String]
}

object FS extends FS[AppM] {
  private def checkFileExists(p: os.Path): IO[Unit] =
    IO(os.exists(p))
      .ifM(ifTrue = IO.unit, ifFalse = IO.raiseError(FileNotFoundErr(p)))

  def readF(p: os.Path): AppM[String] = AppM.liftF(checkFileExists(p) *> IO(os.read(p)))

}
