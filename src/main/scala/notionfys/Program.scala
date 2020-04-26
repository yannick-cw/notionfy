package notionfys

import cats.mtl.ApplicativeAsk
import cats.Monad
import cats.implicits._

object Program {
  def updateNotion[F[_]: Monad](
      implicit FS: FS[F],
      Notion: Notion[F],
      Highlights: Highlights[F],
      Console: Console[F],
      Ask: ApplicativeAsk[F, Args]
  ): F[Unit] =
    for {
      kindlePath                  <- Ask.reader(_.kindle)
      _                           <- Console.verboseLog(s"Reading highlights from Kindle at $kindlePath....")
      kindleFile                  <- FS.readF(kindlePath / "documents" / "My Clippings.txt")
      _                           <- Console.verboseLog("Done readingkindhle highlights")
      kindleHighlights            <- Highlights.parseKindleHighlights(kindleFile)
      _                           <- Console.verboseLog("Parsed kindle highlights to internal format")
      _                           <- Console.verboseLog("Fetching highlights from Notion...")
      (currentHighlights, userId) <- Notion.getHighlights
      _                           <- Console.verboseLog("Fetched current highlghts from Notion")
      _ <- Console.verboseLog(
        s"\nFound Highlights:\n${currentHighlights.map(_.title).mkString("\n")}\n"
      )
      newHighlights = kindleHighlights.filterNot(
        h => currentHighlights.exists(cH => cH.title == h.title && cH.content == h.content)
      )
      _ <- Console.verboseLog(
        s"\nSyncing new highlights:\n${newHighlights.map(_.title).mkString("\n")}"
      )
      _ <- Console.verboseLog("....\n")
      _ <- newHighlights.traverse(Notion.addHighlight(userId))
      _ <- Console.log("Done syncing new highlights to Notion")
      _ <- Console.log("Shutting down")
    } yield ()
}
