package notionfys

import cats.Applicative
import atto._
import Atto._
import cats.implicits._
import cats.data.NonEmptyList

trait Highlights[F[_]] {
  def parseKindleHighlights(f: String): F[List[Highlight]]
}

object Highlights {

  case class ContentPart(title: String, content: String)
  case class TagPart(title: String, tags: NonEmptyList[String])

  def apply[F[_]: Applicative] = new Highlights[F] {
    def parseKindleHighlights(f: String): F[List[Highlight]] =
      Applicative[F].pure(
        pp.parseOnly(f.filterNot(_ == '\r')).either.getOrElse(List.empty).filter(_.content.nonEmpty)
      )

    val whitespace = char(' ')
    val sepP       = string("==========")
    val title      = many1(notChar('\n')).map(_.toList.mkString)
    val lines      = many(notChar('\n')).map(_.mkString)
    val tagP       = many1(letter | digit)

    val tags = (char('.') ~> tagP.map(_.toList.mkString))
      .sepBy1(whitespace | char(',') | char(',') <~ whitespace)

    val eol = char('\n')

    val contentPartParser: Parser[ContentPart] =
      (title <~ eol <~ lines <~ eol <~ lines <~ eol, lines <~ eol).mapN(ContentPart)

    val tagPartParser: Parser[TagPart] =
      (title <~ eol <~ lines <~ eol <~ lines <~ eol, tags <~ eol).mapN(TagPart)

    val segmentP: Parser[Either[(TagPart, ContentPart), ContentPart]] =
      (((tagPartParser <~ sepP <~ eol) ~ contentPartParser || contentPartParser) <~ sepP <~ eol)

    val pp = many(segmentP.map {
      case Right(c)     => Highlight(c.title, c.content, List.empty)
      case Left((t, c)) => Highlight(c.title, c.content, t.tags.toList)
    })
  }
}
