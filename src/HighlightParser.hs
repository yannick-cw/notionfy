{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module HighlightParser
  ( Highlight(..)
  , parseHighlights
  )
where

import           Data.List.Extra                ( notNull )
import           Data.Either.Extra              ( mapLeft )
import           Text.ParserCombinators.Parsec
import           AppErrors

data Highlight = Highlight { title :: String, content :: String} deriving (Show, Eq)

parseHighlights :: String -> Either BlowUp [Highlight]
parseHighlights fileContent = do
  rawHighlights <- mapLeft (ParsErr . show) (parse highlights "" fileContent)
  return
    $   rawHighlights
    >>= (\case
          [t, _, _, highlight, _]
            | notNull t && notNull highlight && (highlight /= "")
            -> [Highlight t highlight]
          _ -> []
        )

 where
  highlights   = endBy oneHighlight eoh
  oneHighlight = sepBy line (string "\r\n")
  line         = many (noneOf "=\r\n")
  eoh          = string "==========\r\n"
