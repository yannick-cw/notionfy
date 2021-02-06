package notionfys

import org.scalatest.{FlatSpec, Matchers}
import cats.Id

class HighlightsSpec extends FlatSpec with Matchers {
  "Program" should "find all highlights" in {
    val clippingsFile                     = os.read(os.resource / "My Clippings.txt")
    val parsedHighlights: List[Highlight] = Highlights[Id].parseKindleHighlights(clippingsFile)
    parsedHighlights should contain.theSameElementsAs(expectedHighlights)
  }

  it should "parse issue3 file" in {
    val clippingsFile                     = os.read(os.resource / "issue_3_clippings.txt")
    val parsedHighlights: List[Highlight] = Highlights[Id].parseKindleHighlights(clippingsFile)
    parsedHighlights should contain.theSameElementsAs(issue3Highlights)
  }

  def expectedHighlights = List(
    Highlight(
      title = "Tools of Titans (Timothy Ferriss)",
      reference =
        "- Ihre Markierung bei Position 6801-6805 | Hinzugefügt am Mittwoch, 9. Oktober 2019 11:13:06",
      content = "brainpicking",
      List.empty
    ),
    Highlight(
      title = "Der Rithmatist: Roman (German Edition) (Sanderson, Brandon)",
      reference =
        "- Ihr Lesezeichen auf Seite 307 | Position 3793 | Hinzugefügt am Mittwoch, 16. Oktober 2019 11:34:47",
      content = "What",
      List("tag1", "tag2", "tag3")
    ),
    Highlight(
      title = "Thinking with Types (Sandy Maguire)",
      reference =
        "- Ihre Markierung bei Position 224-225 | Hinzugefügt am Mittwoch, 6. November 2019 06:10:25",
      content = "Monads are fun",
      tags = List("fp", "is", "good")
    )
  )

  def issue3Highlights =
    List(
      Highlight(
        title = "Indistractable (Nir Eyal;)",
        reference =
          "- Your Highlight on page 69 | Location 1051-1052 | Added on Friday, October 11, 2019 8:50:35 AM",
        content =
          "The Fogg Behavior Model states that for a behavior (B) to occur, three things must be present at the same time: motivation (M), ability (A), and a trigger (T). More succinctly, B = MAT.",
        List.empty
      ),
      Highlight(
        title = "Indistractable (Nir Eyal;)",
        reference =
          "- Your Highlight on page 71 | Location 1084-1085 | Added on Friday, October 11, 2019 9:02:49 AM",
        content = "Is this trigger serving me, or am I serving it?",
        List.empty
      )
    )
}
