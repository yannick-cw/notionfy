import org.scalatest.{FlatSpec, Matchers}

import cats.Id
import notionfys.Highlight
import notionfys.Highlights

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

  val expectedHighlights = List(
    Highlight(title = "Tools of Titans (Timothy Ferriss)", content = "brainpicking", List.empty),
    Highlight(
      title = "Der Rithmatist: Roman (German Edition) (Sanderson, Brandon)",
      content = "What",
      List("tag1", "tag2", "tag3")
    ),
    Highlight(
      title = "Thinking with Types (Sandy Maguire)",
      content = "Monads are fun",
      tags = List("fp", "is", "good")
    )
  )

  val issue3Highlights =
    List(
      Highlight(
        title = "Indistractable (Nir Eyal;)",
        content =
          "The Fogg Behavior Model states that for a behavior (B) to occur, three things must be present at the same time: motivation (M), ability (A), and a trigger (T). More succinctly, B = MAT.",
        List.empty
      ),
      Highlight(
        title = "Indistractable (Nir Eyal;)",
        content = "Is this trigger serving me, or am I serving it?",
        List.empty
      )
    )
}
