module HighlightParserSpec where

import           Test.Hspec
import           HighlightParser

spec :: Spec
spec =
  describe "highlightParser"
    $          it "parses the highlights from file"
    $          parseHighlights exampleFileContent
    `shouldBe` Right expectedHighlights

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
