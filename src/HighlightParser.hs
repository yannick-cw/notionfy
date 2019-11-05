module HighlightParser
  ( Highlight(..)
  , parseHighlights
  )
where

import           Data.List.Split
import           Data.List.Extra                ( notNull )
import           Data.Maybe                     ( maybeToList )

data Highlight = Highlight { title :: String, content :: String} deriving (Show, Eq)

parseHighlights :: String -> [Highlight]
parseHighlights fileContent = s
 where
  s = do
    section <- splitOn "==========\n" fileContent
    maybeToList $ case lines section of
      [t, _, _, highlight] | notNull t && notNull highlight ->
        Just $ Highlight t highlight
      _ -> Nothing


