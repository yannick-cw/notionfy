package notionfys

import cats.effect._
import cats.implicits._
import cats.mtl.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import App._
import scala.util.control.NonFatal
import sttp.client.DeserializationError
import sttp.client.HttpError

case class Highlight(title: String, content: String, tags: List[String])

object Main
    extends CommandIOApp(
      name = "notionfy",
      header = "Sync your Kindle highlights to Notion",
      version = "0.2.5"
    ) {

  implicit val F: FS[AppM]         = FS
  implicit val N: Notion[AppM]     = Notion
  implicit val H: Highlights[AppM] = Highlights[AppM]
  implicit val C: Console[AppM]    = Console

  private def errorMsg(errShort: String, errLong: String, args: Args): IO[ExitCode] =
    IO(
      println(
        s"\n\nUnfortunately syncing failed, you can run with --verbose to see more details about what is going on. Please report the output to https://github.com/yannick-cw/notionfys, so I can fix it. Here is the error:\n\n${if (args.verbose)
          errLong
        else errShort}"
      )
    ).as(ExitCode.Error)

  private def errorMsg(err: Throwable, args: Args): IO[ExitCode] =
    errorMsg(
      err.getMessage,
      err.getLocalizedMessage ++ "\n" ++ formatTrace(err.getStackTrace),
      args
    )

  private def formatTrace(trace: Array[StackTraceElement]) = trace.mkString("\n")

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
              case HttpError(b) =>
                errorMsg(
                  "Http request to Notion failed",
                  "Http request to Notion failed with:\n" ++ b,
                  args
                )
              case err @ DeserializationError(b, e) =>
                errorMsg(
                  e.toString,
                  e.toString ++ "\nwith Stacktrace:\n" ++ formatTrace(err.getStackTrace)
                    ++ "\nwith response body:\n" ++ b,
                  args
                )
              case FileNotFoundErr(p) =>
                val notFoundErr = s"Did not find the kindle clippings file under the path $p"
                errorMsg(notFoundErr, notFoundErr, args)
              case HttpResponseError(code, body) =>
                errorMsg(
                  errShort = s"Http request failed with status code: $code",
                  errLong = s"Http request failed with status code: $code and body: $body",
                  args = args
                )
              case DecodingError(msg, body) =>
                errorMsg(
                  errShort = s"Http request failed while decoding the response with: $msg",
                  errLong =
                    s"Http request failed while decoding the response with: $msg and body: $body",
                  args = args
                )
              case NonFatal(err) => errorMsg(err, args)
            }
      )
}
