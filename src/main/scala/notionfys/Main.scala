package notionfys

import cats.effect._
import cats.implicits._
import cats.mtl.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import App._
import scala.util.control.NonFatal

case class Highlight(title: String, content: String, tags: List[String])

object Main
    extends CommandIOApp(
      name = "notionfy",
      header = "Sync your Kindle highlights to Notion",
      version = "0.0.0"
    ) {

  implicit val F: FS[AppM]         = FS
  implicit val N: Notion[AppM]     = Notion
  implicit val H: Highlights[AppM] = Highlights[AppM]
  implicit val C: Console[AppM]    = Console

  private def errorMsg(err: Throwable, args: Args) =
    s"Unfortunately syncing failed, you can run with --verbose to see more details about what is going on. Please report the output to https://github.com/yannick-cw/notionfys, so I can fix it. Here is the raw error:\n${if (args.verbose)
      err.getLocalizedMessage ++ "\n" ++ err.getStackTrace.mkString("\n")
    else err.getMessage}"

  override def main: Opts[IO[ExitCode]] =
    Cli.parseArgs
      .map(
        args =>
          Program
            .updateNotion[AppM]
            .run(args)
            .as(println("Welcome"))
            .as(ExitCode.Success)
            .recoverWith {
              case NonFatal(err) => IO(println(errorMsg(err, args))).as(ExitCode.Error)
            }
      )
}
