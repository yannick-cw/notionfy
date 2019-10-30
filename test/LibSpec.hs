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
                 | AddPage String String
                 | GetPage String String
                 | ParseKindle String
                 | ParseNotion String deriving (Show, Eq)

instance FS TestApp where
  readF path = tell [FSPath path] $> "kindle_file_content"

instance Notion TestApp where
  addSubPage highlight = do
    args <- ask
    tell [AddPage (notionId args) (title highlight)]
  getSubPages (PageId title) = do
    args <- ask
    tell [GetPage (notionId args) title] $> ["subP1_content"]

instance Highlights TestApp where
  parseKindleHighlights fileContent =
    tell [ParseKindle fileContent]
      $> [ Highlight { author = "au", title = "title", content = "content" }
         , Highlight { author = "au2", title = "title2", content = "content2" }
         , Highlight { author = "au3", title = "title3", content = "content3" }
         ]
  parseNotionHighlight pageContent =
    tell [ParseNotion pageContent]
      $> Highlight { author = "au2", title = "title2", content = "content2" }


runTest :: TestApp ()
runTest = updateNotion

spec :: Spec
spec = describe "updateNotion" $ do
  it "executes the expected commands in order"
    $          writtenCommands
    `shouldBe` [ FSPath "pathToHighlightFile"
               , ParseKindle "kindle_file_content"
               , GetPage "theNotionId" "parentPageId"
               , ParseNotion "subP1_content"
               , AddPage "theNotionId" "title"
               , AddPage "theNotionId" "title3"
               ]
  it "does not add the in notion existing page"
    $          any (\command -> command == AddPage "theNotionId" "title2")
                   writtenCommands
    `shouldBe` False
 where
  args :: Args
  args = Args { notionId       = "theNotionId"
              , parentPageId   = "parentPageId"
              , highlightsPath = "pathToHighlightFile"
              }
  writtenCommands :: [TestCommand]
  writtenCommands = runReader (execWriterT $ runExceptT $ unpack runTest) args

