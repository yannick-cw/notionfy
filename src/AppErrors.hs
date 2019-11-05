module AppErrors where

data BlowUp = ParsErr String | FsErr String |  NotionErr String  deriving (Show)
