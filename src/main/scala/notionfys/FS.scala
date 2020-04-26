package notionfys

import App._
import cats.effect.IO

trait FS[F[_]] {
  def readF(p: os.Path): F[String]
}
object FS extends FS[AppM] {
  def readF(p: os.Path): AppM[String] = AppM.liftF(IO(os.read(p)))
}
