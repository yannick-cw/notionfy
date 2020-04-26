package notionfys

import cats.data.ValidatedNel
import cats.data.NonEmptyList
import cats.data.Validated
import scala.util.Try
import java.{util => ju}
import com.monovore.decline._
import cats.implicits._

case class Args(
    token: String,
    page: ju.UUID,
    kindle: os.Path,
    verbose: Boolean = false
)

object Cli {
  def parseArgs: Opts[Args] =
    (
      Opts.option[String](
        "token",
        short = "n",
        metavar = "id",
        help = "Your notion token, found in the token_v2 cookie when you open notion in the browser"
      ),
      Opts
        .option[String](
          "page",
          short = "p",
          metavar = "id",
          help =
            "Id of the page to which the highlights should be added, you find it in the url if you open the page in the browser"
        )
        .mapValidated(parseToUUID),
      Opts
        .option[String](
          "kindle",
          short = "k",
          metavar = "Path",
          help = "Path to your kindle, e.g. on Mac /Volumes/Kindle"
        )
        .mapValidated(
          p =>
            Validated
              .fromTry(Try(os.Path(p)))
              .bimap(_ => NonEmptyList.one(s"$p is not an absolute path"), identity)
        ),
      Opts.flag("verbose", help = "turn on verbose logging").orFalse
    ).mapN(Args)

  private def parseToUUID(rawId: String): ValidatedNel[String, ju.UUID] =
    Validated
      .fromTry(Try(ju.UUID.fromString(rawId.zipWithIndex.flatMap {
        case (c, p) if p == 7 || p == 11 || p == 15 || p == 19 => List(c, '-')
        case (c, _)                                            => List(c)
      }.mkString)))
      .bimap(_ => NonEmptyList.one(s"$rawId was is not parsebble to UUID"), identity)

}
