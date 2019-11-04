{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NotionClient
  ( loadUserId
  , writeHighlight
  )
where

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
import           Data.Aeson                     ( toJSON
                                                , encode
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( toList )
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


notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

loadUserId :: ExceptT String (ReaderT String IO) String
loadUserId = do
  opts   <- lift cookieOpts
  r      <- liftIO $ postWith opts (notionUrl ++ "loadUserContent") emptyObject
  userId <-
    case
      (r ^? responseBody . key "recordMap" . key "notion_user" . _Object)
        >>= (listToMaybe . keys)
    of
      Just j  -> return j
      Nothing -> throwError
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
  :: Highlight -> String -> String -> ExceptT String (ReaderT String IO) ()
writeHighlight highlight parentPageId userId = do
  (headerId, contentId, seperatorId) <- liftIO
    $ liftA3 (,,) randomIO randomIO randomIO
  opts <- lift cookieOpts
  now  <- liftIO getCurrentTime
  let transaction =
        Transaction { operations = header ++ contentPart ++ seperator }
      header =
        [ addSegment headerId "sub_header"
        , addAfter headerId
        , addContent headerId (title highlight)
        ]
      contentPart =
        [ addSegment contentId "text"
        , addAfter contentId
        , addContent contentId (content highlight)
        ]
      seperator = [addSegment seperatorId "divider", addAfter seperatorId]
  void $ liftIO $ postWith opts
                           (notionUrl ++ "submitTransaction")
                           (toJSON transaction)
 where
  addAfter opId = Operation
    { NotionClient.id = parentPageId
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
  addSegment opId _type = Operation
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
                            , ("parent_id"   , S parentPageId)
                            , ("parent_table", S "block")
                            , ("type"        , S _type)
                            ]
                          )
    }

cookieOpts :: ReaderT String IO Options
cookieOpts = do
  token <- ask
  now   <- liftIO getCurrentTime
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
