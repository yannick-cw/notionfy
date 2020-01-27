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

import           AppErrors
import           Control.Monad.Except           ( throwError
                                                , MonadIO
                                                , ExceptT
                                                , MonadError
                                                , liftIO
                                                , runExceptT
                                                , ExceptT(..)
                                                )
import           Control.Exception
import           Data.Text                      ( unpack )
import           Control.Monad.Reader           ( MonadReader
                                                , ReaderT
                                                , asks
                                                , runReaderT
                                                )
import           Data.Foldable                  ( traverse_ )
import           CliParser                      ( Args(..)
                                                , parseArgs
                                                )
import           HighlightParser
import           System.Exit                    ( exitFailure )
import           System.FilePath                ( (</>) )
import           NotionClient
import           Data.ByteString               as BS
                                                ( readFile )
import           Data.Text.Encoding             ( decodeUtf8 )


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
    tryReading = try $ unpack . decodeUtf8 <$> BS.readFile path

instance Notion AppM where
  addSubPage highlight = AppM $ writeHighlight highlight
  getSubPages = AppM getHighlights

instance Highlights AppM where
  parseKindleHighlights = AppM . ExceptT . return . parseHighlights

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

class (MonadError BlowUp m) => FS m where
  readF:: String -> m String

class (MonadError BlowUp m) => Highlights m where
  parseKindleHighlights :: String -> m [Highlight]

newtype PageId = PageId String deriving (Show)
newtype Content = Content String
class (MonadError BlowUp m, MonadReader Args m) => Notion m where
  addSubPage :: Highlight -> m ()
  getSubPages :: m [Highlight]

updateNotion :: (FS m, Notion m, Highlights m, MonadReader Args m) => m ()
updateNotion = do
  kindlePath        <- asks highlightsPath
  kindleFile        <- readF $ kindlePath </> "documents" </> "My Clippings.txt"
  kindleHighlights  <- parseKindleHighlights kindleFile
  currentHighlights <- getSubPages
  let newHighlighs = filter
        (\h -> not $ any
          (\cH -> title cH == title h && content cH == content h)
          currentHighlights
        )
        kindleHighlights
  traverse_ addSubPage newHighlighs


