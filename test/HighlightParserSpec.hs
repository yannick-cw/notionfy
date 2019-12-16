module HighlightParserSpec where

import           Test.Hspec
import           HighlightParser

spec :: Spec
spec = describe "highlightParser" $ do
  it "parses the highlights from file"
    $          parseHighlights exampleFileContent
    `shouldBe` Right expectedHighlights
  it "parses a windows highlights file"
    $          parseHighlights winExampleFileContent
    `shouldBe` Right expectedHighlights
  it "parses file from issue3"
    $          parseHighlights issue3Example
    `shouldBe` Right issue3Highlights

expectedHighlights =
  [ Highlight { title = "Blindsight (Peter Watts)", content = "chaff" }
  , Highlight { title   = "Tools of Titans (Timothy Ferriss)"
              , content = "brainpicking"
              }
  , Highlight { title   = "Tools of Titans (Timothy Ferriss)"
              , content = "What is the best or most"
              }
  ]

issue3Highlights =
  [ Highlight
    { title   = "Indistractable (Nir Eyal;)"
    , content =
      "The Fogg Behavior Model states that for a behavior (B) to occur, three things must be present at the same time: motivation (M), ability (A), and a trigger (T). More succinctly, B = MAT."
    }
  , Highlight { title   = "Indistractable (Nir Eyal;)"
              , content = "Is this trigger serving me, or am I serving it?"
              }
  ]

exampleFileContent =
  "Das Jesus-Video: Thriller (German Edition) (Eschbach, Andreas)\r\n\
\- Ihr Lesezeichen bei Position 1420 | Hinzugefügt am Samstag, 3. Januar 2015 19:14:54\r\n\
\\r\n\
\\r\n\
\a==========\r\n\
\Blindsight (Peter Watts)\r\n\
\- Ihre Markierung bei Position 1446-1446 | Hinzugefügt am Sonntag, 7. Mai 2017 22:26:04\r\n\
\\r\n\
\chaff\r\n\
\==========\r\n\
\Tools of Titans (Timothy Ferriss)\r\n\
\- Ihre Notiz bei Position 6805 | Hinzugefügt am Mittwoch, 9. Oktober 2019 11:13:06\r\n\
\\r\n\
\brainpicking\r\n\
\==========\r\n\
\Tools of Titans (Timothy Ferriss)\r\n\
\- Ihre Markierung bei Position 6813-6813 | Hinzugefügt am Mittwoch, 9. Oktober 2019 11:14:03\r\n\
\\r\n\
\What is the best or most\r\n\
\==========\r\n\
\Der Rithmatist: Roman (German Edition) (Sanderson, Brandon)\r\n\
\- Ihr Lesezeichen auf Seite 307 | Position 3793 | Hinzugefügt am Mittwoch, 16. Oktober 2019 11:34:47\r\n\
\\r\n\
\\r\n\
\==========\r\n"

winExampleFileContent =
  "Das Jesus-Video: Thriller (German Edition) (Eschbach, Andreas)\n\
\- Ihr Lesezeichen bei Position 1420 | Hinzugefügt am Samstag, 3. Januar 2015 19:14:54\n\
\\n\
\\n\
\a==========\n\
\Blindsight (Peter Watts)\n\
\- Ihre Markierung bei Position 1446-1446 | Hinzugefügt am Sonntag, 7. Mai 2017 22:26:04\n\
\\n\
\chaff\n\
\==========\n\
\Tools of Titans (Timothy Ferriss)\n\
\- Ihre Notiz bei Position 6805 | Hinzugefügt am Mittwoch, 9. Oktober 2019 11:13:06\n\
\\n\
\brainpicking\n\
\==========\n\
\Tools of Titans (Timothy Ferriss)\n\
\- Ihre Markierung bei Position 6813-6813 | Hinzugefügt am Mittwoch, 9. Oktober 2019 11:14:03\n\
\\n\
\What is the best or most\n\
\==========\n\
\Der Rithmatist: Roman (German Edition) (Sanderson, Brandon)\n\
\- Ihr Lesezeichen auf Seite 307 | Position 3793 | Hinzugefügt am Mittwoch, 16. Oktober 2019 11:34:47\n\
\\n\
\\n\
\==========\n"

issue3Example =
  "Indistractable (Nir Eyal;)\n\
\- Your Highlight on page 69 | Location 1051-1052 | Added on Friday, October 11, 2019 8:50:35 AM\n\
\\n\
\The Fogg Behavior Model states that for a behavior (B) to occur, three things must be present at the same time: motivation (M), ability (A), and a trigger (T). More succinctly, B = MAT.\n\
\==========\n\
\Indistractable (Nir Eyal;)\n\
\- Your Highlight on page 71 | Location 1084-1085 | Added on Friday, October 11, 2019 9:02:49 AM\n\
\\n\
\Is this trigger serving me, or am I serving it?\n\
\==========\n"
