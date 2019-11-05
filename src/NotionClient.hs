{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module NotionClient
  ( loadUserId
  , writeHighlight
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
import           Data.Aeson.Types               ( emptyObject
                                                , ToJSON
                                                , FromJSON(..)
                                                , sumEncoding
                                                , unwrapUnaryRecords
                                                , genericToJSON
                                                , fieldLabelModifier
                                                , defaultOptions
                                                , SumEncoding(..)
                                                )
import           Data.Aeson.Lens                ( _Object
                                                , key
                                                )
import           Data.Aeson                     ( toJSON
                                                , genericParseJSON
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe
                                                , catMaybes
                                                , maybeToList
                                                )
import           Data.HashMap.Strict            ( keys
                                                , HashMap
                                                , fromList
                                                )
import           Data.UUID
import           Data.List.Split                ( splitWhen )
import           Control.Lens
import           Control.Applicative            ( liftA3 )
import           HighlightParser                ( Highlight(..) )
import           GHC.Generics
import           System.Random                  ( randomIO )
import           CliParser                      ( HasNotion(..) )


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

loadUserId :: HasNotion r => ExceptT BlowUp (ReaderT r IO) String
loadUserId = do
  opts   <- lift cookieOpts
  r      <- liftIO $ postWith opts (notionUrl ++ "loadUserContent") emptyObject
  userId <-
    case
      (r ^? responseBody . key "recordMap" . key "notion_user" . _Object)
        >>= (listToMaybe . keys)
    of
      Just j  -> return j
      Nothing -> throwError $ NotionErr
        "Notion responded not with expected Json, .recordMap.notion_user.{id}"
  return $ unpack userId


data StringOrNum = S String | N Int deriving (Generic)
instance ToJSON StringOrNum where
  toJSON = genericToJSON
    (defaultOptions { sumEncoding = UntaggedValue, unwrapUnaryRecords = True })

data Arg =  ArrayArgs [[String]]  | ObjArgs ( HashMap String StringOrNum ) deriving (Generic)
instance ToJSON Arg where
  toJSON = genericToJSON
    (defaultOptions { sumEncoding = UntaggedValue, unwrapUnaryRecords = True })


data Operation = Operation { id :: String, path :: [String], command :: String, table :: String, args :: Arg } deriving (Generic)
instance ToJSON Operation

newtype Transaction = Transaction { operations :: [ Operation ] } deriving (Generic)
instance ToJSON Transaction

writeHighlight
  :: HasNotion r => Highlight -> String -> ExceptT BlowUp (ReaderT r IO) ()
writeHighlight highlight userId = do
  (headerId, contentId, seperatorId) <- liftIO
    $ liftA3 (,,) randomIO randomIO randomIO
  (_, parentPageId) <- asks notionConf
  opts              <- lift cookieOpts
  now               <- liftIO getCurrentTime
  let transaction =
        Transaction { operations = header ++ contentPart ++ seperator }
      header =
        [ addSegment headerId "sub_header" parentPageId
        , addAfter headerId parentPageId
        , addContent headerId ((title :: Highlight -> String) highlight)
        ]
      contentPart =
        [ addSegment contentId "text" parentPageId
        , addAfter contentId parentPageId
        , addContent contentId ((content :: Highlight -> String) highlight)
        ]
      seperator =
        [ addSegment seperatorId "divider" parentPageId
        , addAfter seperatorId parentPageId
        ]
  void $ liftIO $ postWith opts
                           (notionUrl ++ "submitTransaction")
                           (toJSON transaction)
 where
  addAfter opId parentId = Operation
    { NotionClient.id = parentId
    , path            = ["content"]
    , command         = "listAfter"
    , table           = "block"
    , args            = ObjArgs (fromList [("id", S $ toString opId)])
    }
  addContent opId content = Operation { NotionClient.id = toString opId
                                      , path = ["properties", "title"]
                                      , command         = "set"
                                      , table           = "block"
                                      , args            = ArrayArgs [[content]]
                                      }
  addSegment opId _type parentId = Operation
    { NotionClient.id = toString opId
    , path            = []
    , command         = "set"
    , table           = "block"
    , args            = ObjArgs
                          (fromList
                            [ ("id"          , S $ toString opId)
                            , ("version"     , N 1)
                            , ("alive"       , S "True")
                            , ("created_by"  , S userId)
                            , ("parent_id"   , S parentId)
                            , ("parent_table", S "block")
                            , ("type"        , S _type)
                            ]
                          )
    }

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
  let highlightGroups = splitWhen ((==) "divider" . _type) subResults
      withProperties  = (>>= (maybeToList . properties)) <$> highlightGroups
      titles          = ((title :: Props -> [[String]]) <$>) <$> withProperties
      highlights =
        titles
          >>= ( maybeToList
              . (\case
                  [[[title]], [[content]]] ->
                    Just $ Highlight { title = title, content = content }
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
      cookie  = Cookie "token_v2"
                       (pack token)
                       expires
                       "notion.so"
                       "/"
                       now
                       now
                       False
                       False
                       True
                       True
      jar   = createCookieJar [cookie]
      opts  = defaults :: Options
      opts' = opts & cookies ?~ jar
  return opts'
