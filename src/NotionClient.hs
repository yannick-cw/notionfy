{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NotionClient
  ( loadUserId
  , writeHighlight
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
                                                , sumEncoding
                                                , unwrapUnaryRecords
                                                , genericToJSON
                                                , defaultOptions
                                                , SumEncoding(..)
                                                )
import           Data.Aeson.Lens                ( _Object
                                                , key
                                                )
import           Data.Aeson                     ( toJSON )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe )
import           Data.HashMap.Strict            ( keys
                                                , HashMap
                                                , fromList
                                                )
import           Data.UUID
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

data Arg =  Args [[String]]  | ObjArgs ( HashMap String StringOrNum ) deriving (Generic)
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
        , addContent headerId (title highlight)
        ]
      contentPart =
        [ addSegment contentId "text" parentPageId
        , addAfter contentId parentPageId
        , addContent contentId (content highlight)
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
                                      , args            = Args [[content]]
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
