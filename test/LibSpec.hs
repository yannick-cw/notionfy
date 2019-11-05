{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LibSpec where

import           Test.Hspec
import           Lib
import           Control.Monad.Except
import           Control.Monad.Writer
import           Control.Monad.Reader           ( MonadReader
                                                , Reader
                                                , runReader
                                                , ask
                                                )
import           Data.Functor
import           CliParser                      ( Args(..) )

newtype TestApp a = TestApp { unpack :: ExceptT BlowUp (WriterT [TestCommand] (Reader Args) ) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadWriter [TestCommand]
    , MonadReader Args
    , MonadError BlowUp
  )

data TestCommand = CliArg String
                 | FSPath String
                 | AddPage String String String
                 | GetPage String String
                 | ParseKindle String
                 | ParseNotion String deriving (Show, Eq)

instance FS TestApp where
  readF path = tell [FSPath path] $> "kindle_file_content"

instance Notion TestApp where
  addSubPage highlight = do
    args <- ask
    tell [AddPage (notionId args) (title highlight) (content highlight)]
  getSubPages  = do
    args <- ask
    tell [GetPage (notionId args) ( parentPageId args )]
      $> [Highlight { title = "title2", content = "content2" }]

instance Highlights TestApp where
  parseKindleHighlights fileContent =
    tell [ParseKindle fileContent]
      $> [ Highlight { title = "title", content = "content" }
         , Highlight { title = "title2", content = "content2" }
         , Highlight { title = "title2", content = "content2_diff" }
         , Highlight { title = "title3", content = "content3" }
         ]


runTest :: TestApp ()
runTest = updateNotion

spec :: Spec
spec = describe "updateNotion" $ do
  it "executes the expected commands in order"
    $          writtenCommands
    `shouldBe` [ FSPath "pathToKindle/documents/My Clippings.txt"
               , ParseKindle "kindle_file_content"
               , GetPage "theNotionId" "parentPageId"
               , AddPage "theNotionId" "title"  "content"
               , AddPage "theNotionId" "title2" "content2_diff"
               , AddPage "theNotionId" "title3" "content3"
               ]
  it "does not add the in notion existing page"
    $ any (\command -> command == AddPage "theNotionId" "title2" "content2")
          writtenCommands
    `shouldBe` False
  it "does add a page for different content but existing title"
    $          any
                 (\command -> command == AddPage "theNotionId" "title2" "content2_diff")
                 writtenCommands
    `shouldBe` True
 where
  args :: Args
  args = Args { notionId       = "theNotionId"
              , parentPageId   = "parentPageId"
              , highlightsPath = "pathToKindle"
              }
  writtenCommands :: [TestCommand]
  writtenCommands = runReader (execWriterT $ runExceptT $ unpack runTest) args

