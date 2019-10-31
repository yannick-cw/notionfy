{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Monad.Except           ( throwError
                                                , MonadIO
                                                , ExceptT
                                                , MonadError
                                                , liftIO
                                                , runExceptT
                                                )
import           Control.Exception
import           System.IO
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
import           HighlightParser
import           System.Exit                    ( exitFailure )
import           Debug.Trace                    ( trace )


newtype AppM a = AppM { unWrapAppM :: ExceptT BlowUp (ReaderT Args IO) a }
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadReader Args
    , MonadIO
    , MonadError BlowUp
  )

instance FS AppM where
  readF path = liftIO tryReading >>= \case
    Right text -> pure text
    Left  err  -> throwError $ FsErr $ show err
   where
    tryReading :: IO (Either IOError String)
    tryReading = try $ withFile path ReadMode hGetContents

instance Notion AppM where
  addSubPage  = liftIO . putStr . show
  getSubPages = liftIO . ($> ["SubPage"]) . putStr . show

instance Highlights AppM where
  parseKindleHighlights = pure . parseHighlights
  parseNotionHighlight =
    liftIO
      . ($> Highlight { title = "title", content = "content" })
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


