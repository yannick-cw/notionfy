module HighlightParserSpec where

import           Test.Hspec
import           HighlightParser

spec :: Spec
spec =
  describe "highlightParser"
    $          it "parses the highlights from file"
    $          parseHighlights exampleFileContent
    `shouldBe` expectedHighlights

expectedHighlights =
  [ Highlight { title = "Blindsight (Peter Watts)", content = "chaff" }
  , Highlight { title   = "Tools of Titans (Timothy Ferriss)"
              , content = "brainpicking"
              }
  , Highlight { title   = "Tools of Titans (Timothy Ferriss)"
              , content = "What is the best or most"
              }
  ]

exampleFileContent =
  "Das Jesus-Video: Thriller (German Edition) (Eschbach, Andreas)\n\
\- Ihr Lesezeichen bei Position 1420 | Hinzugefügt am Samstag, 3. Januar 2015 19:14:54\n\
\\n\
\\n\
\==========\n\
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
