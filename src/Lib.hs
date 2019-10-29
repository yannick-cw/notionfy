{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( runUpdate
  , updateNotion
  , BlowUp(..)
  , Cli(..)
  , FS(..)
  , Highlights(..)
  , Highlight(..)
  , PageId(..)
  , Content(..)
  , NotionId(..)
  , Notion(..)
  )
where

import           Control.Monad.Except
import           Control.Applicative            ( liftA3 )
import           Control.Monad.Reader           ( Reader
                                                , runReader
                                                )
import           Data.Foldable                  ( traverse_ )


runUpdate :: IO ()
runUpdate = putStrLn "someFunc"

data BlowUp = ParsErr String | FsErr String |  NotionErr String

class (MonadError BlowUp m) => Cli m where
  getArg :: String -> m String

class (MonadError BlowUp m) => FS m where
  readF:: String -> m String

data Highlight = Highlight { author :: String, title :: String, content :: String}
class (MonadError BlowUp m) => Highlights m where
  parseKindleHighlights :: String -> m [Highlight]
  parseNotionHighlight :: String -> m Highlight

newtype PageId = PageId String
newtype Content = Content String
newtype NotionId = NotionId String
class (MonadError BlowUp m) => Notion m where
  addSubPage :: NotionId -> Highlight -> m ()
  getSubPages :: NotionId -> PageId -> m [String]

updateNotion :: (Cli m, FS m, Notion m, Highlights m) => m ()
updateNotion = do
  (id, parentPageId, highlightsPath) <- liftA3 (,,)
                                               (getArg "notionId")
                                               (getArg "pageId")
                                               (getArg "highlightFile")
  kindleFile       <- readF highlightsPath
  kindleHighlights <- parseKindleHighlights kindleFile
  let notionId = NotionId id
  subPages          <- getSubPages notionId (PageId parentPageId)
  currentHighlights <- traverse parseNotionHighlight subPages
  let newHighlighs = filter
        (\h -> not $ any (\cH -> title cH == title h) currentHighlights)
        kindleHighlights
  traverse_ (addSubPage notionId) newHighlighs


