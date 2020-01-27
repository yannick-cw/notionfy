{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module NotionClient
  ( writeHighlight
  , getHighlights
  )
where

import           AppErrors
import           Network.Wreq
import           Network.HTTP.Client            ( createCookieJar
                                                , Cookie(Cookie)
                                                )
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON(..)
                                                , fieldLabelModifier
                                                , defaultOptions
                                                )
import           Data.Aeson                     ( toJSON
                                                , genericParseJSON
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8         as C
                                                ( pack )
import           Data.Text                     as T
                                                ( pack )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , maybeToList
                                                )
import           Data.UUID
import           Data.List.Split                ( splitWhen )
import           Control.Lens
import           HighlightParser                ( Highlight(..) )
import           GHC.Generics
import           CliParser                      ( HasNotion(..) )
import           NotionApi.High
import           NotionApi.Types               as N
                                                ( runNotionM
                                                , Action(..)
                                                , Element(..)
                                                )




notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

writeHighlight :: HasNotion r => Highlight -> ExceptT BlowUp (ReaderT r IO) ()
writeHighlight highlight = do
  (token, pId) <- asks notionConf
  parentId     <- maybe (throwError (ParsErr "Parent Page Id is no uuid"))
                        return
                        (fromString pId)
  liftIO $ runNotionM
    (T.pack token)
    (runAction
      [ AppendElement (SubHeader (T.pack $ HighlightParser.title highlight))
                      parentId
      , AppendElement
        (TextContent (T.pack $ HighlightParser.content highlight))
        parentId
      , AppendElement Divider parentId
      ]
    )

newtype Props = Props { title :: [[String]]} deriving (Generic)
instance FromJSON Props where

data Value = Value { content :: Maybe [String], _type :: String, properties :: Maybe Props } deriving (Generic)
instance FromJSON Value where
  parseJSON = genericParseJSON
    (defaultOptions { fieldLabelModifier = adjustTypeName })
   where
    adjustTypeName "_type" = "type"
    adjustTypeName n       = n

newtype Results = Results { value :: Value } deriving (Generic)
instance FromJSON Results

newtype PageRes = PageRes { results :: [ Results ] } deriving (Generic)
instance FromJSON PageRes

data Request = Request { table :: String, id :: String} deriving (Generic)
instance ToJSON Request
newtype HighlightReq = HighlightReq { requests :: [ Request ]} deriving (Generic)
instance ToJSON HighlightReq

getHighlights :: HasNotion r => ExceptT BlowUp (ReaderT r IO) [Highlight]
getHighlights = do
  opts           <- lift cookieOpts
  (_, parentId)  <- asks notionConf
  parentValueObj <- liftIO $ getRecords opts parentId
  let contentIds =
        maybe [] (concat . (content :: Value -> Maybe [String])) parentValueObj
  subResults <- liftIO $ catMaybes <$> traverse (getRecords opts) contentIds
  let highlightGroups =
        splitWhen ((==) "divider" . NotionClient._type) subResults
      withProperties = (>>= (maybeToList . properties)) <$> highlightGroups
      titles         = ((title :: Props -> [[String]]) <$>) <$> withProperties
      highlights =
        titles
          >>= ( maybeToList
              . (\case
                  [[[t]], [[c]]] ->
                    Just $ Highlight { title = t, content = c }
                  _ -> Nothing
                )
              )
  return highlights
 where
  getRecords opts parentId = do
    response <- postWith opts
                         (notionUrl ++ "getRecordValues")
                         (toJSON $ requestBody parentId)
    response' <- asJSON response
    let pageRes  = response' ^. responseBody
        firstRes = listToMaybe $ results pageRes
        valueObj = value <$> firstRes
    return valueObj
  requestBody parentId =
    HighlightReq { requests = [Request { table = "block", id = parentId }] }




cookieOpts :: HasNotion r => ReaderT r IO Options
cookieOpts = do
  (token, _) <- asks notionConf
  now        <- liftIO getCurrentTime
  let expires = addUTCTime (1440 * 3000) now
      c       = Cookie "token_v2"
                       (C.pack token)
                       expires
                       "notion.so"
                       "/"
                       now
                       now
                       False
                       False
                       True
                       True
      jar   = createCookieJar [c]
      opts  = defaults :: Options
      opts' = opts & cookies ?~ jar
  return opts'
