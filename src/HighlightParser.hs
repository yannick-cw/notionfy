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
  rawHighlights <- mapLeft (ParsErr . show)
                           (parse highlights "" (removeReturn fileContent))
  return
    $   rawHighlights
    >>= (\case
          [t, _, _, highlight, _]
            | notNull t && notNull highlight && (highlight /= "")
            -> [Highlight t highlight]
          _ -> []
        )

 where
  removeReturn   = filter (/= '\r')
  highlights     = endBy oneHighlight eoh
  oneHighlight   = sepBy line eol
  line           = manyTill anyChar (noHighlightSep <|> noEol)
  noHighlightSep = try $ lookAhead eoh
  noEol          = try $ lookAhead eol
  eoh            = string "==========\n"
  eol            = string "\n"
