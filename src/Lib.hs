{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib
  ( runUpdate
  , updateNotion
  , BlowUp(..)
  , FS(..)
  , Highlights(..)
  , Highlight(..)
  , PageId(..)
  , Content(..)
  , Notion(..)
  , AppM(..)
  )
where

import           Control.Monad.Except
import           Control.Applicative            ( liftA3 )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , ask
                                                , runReaderT
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Functor
import           CliParser                      ( Args(..)
                                                , parseArgs
                                                )
import           Options.Applicative
import           System.Exit                    ( exitFailure )


newtype AppM a = AppM { unWrapAppM :: ExceptT BlowUp (ReaderT Args IO) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadReader Args
    , MonadIO
    , MonadError BlowUp
  )

-- dummy instances for now
instance FS AppM where
  readF = liftIO . ($> "File") . putStr

instance Notion AppM where
  addSubPage  = liftIO . putStr . show
  getSubPages = liftIO . ($> ["SubPage"]) . putStr . show

instance Highlights AppM where
  parseKindleHighlights =
    liftIO
      . ($> [ Highlight { author  = "au2"
                        , title   = "title2"
                        , content = "content2"
                        }
            ]
        )
      . putStr
      . show
  parseNotionHighlight =
    liftIO
      . ($> Highlight { author = "au", title = "title", content = "content" })
      . putStr
      . show

runUpdate :: IO ()
runUpdate = do
  args <- parseArgs
  res  <- runApp args
  case res of
    Right _   -> return ()
    Left  err -> putStr (show err) *> exitFailure
 where
  runApp :: Args -> IO (Either BlowUp ())
  runApp = runReaderT (runExceptT $ unWrapAppM (updateNotion :: AppM ()))

data BlowUp = ParsErr String | FsErr String |  NotionErr String  deriving (Show)

class (MonadError BlowUp m) => FS m where
  readF:: String -> m String

data Highlight = Highlight { author :: String, title :: String, content :: String} deriving (Show)
class (MonadError BlowUp m) => Highlights m where
  parseKindleHighlights :: String -> m [Highlight]
  parseNotionHighlight :: String -> m Highlight

newtype PageId = PageId String deriving (Show)
newtype Content = Content String
class (MonadError BlowUp m, MonadReader Args m) => Notion m where
  addSubPage :: Highlight -> m ()
  getSubPages :: PageId -> m [String]

updateNotion :: (FS m, Notion m, Highlights m, MonadReader Args m) => m ()
updateNotion = do
  (Args _ parentPageId highlightsPath) <- ask
  kindleFile                           <- readF highlightsPath
  kindleHighlights                     <- parseKindleHighlights kindleFile
  subPages                             <- getSubPages (PageId parentPageId)
  currentHighlights                    <- traverse parseNotionHighlight subPages
  let newHighlighs = filter
        (\h -> not $ any (\cH -> title cH == title h) currentHighlights)
        kindleHighlights
  traverse_ addSubPage newHighlighs


