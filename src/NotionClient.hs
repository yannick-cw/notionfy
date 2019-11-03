{-# LANGUAGE OverloadedStrings #-}

module NotionClient
  ( loadUserId
  )
where

import           Network.Wreq
import           Network.HTTP.Client            ( createCookieJar
                                                , Cookie(Cookie)
                                                )
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Aeson.Types               ( emptyObject )
import           Data.Aeson.Lens                ( _Object
                                                , key
                                                )
import           Data.Time.Clock
import           Data.ByteString.Char8          ( pack )
import           Data.Text                      ( unpack )
import           Data.Maybe                     ( listToMaybe )
import           Data.Foldable                  ( toList )
import           Data.HashMap.Strict            ( keys )
import           Control.Lens

notionUrl :: String
notionUrl = "https://www.notion.so/api/v3/"

loadUserId :: ExceptT String (ReaderT String IO) String
loadUserId = do
  notionToken <- ask
  opts        <- liftIO $ cookieOpts notionToken
  r <- liftIO $ postWith opts (notionUrl ++ "loadUserContent") emptyObject
  userId      <-
    case
      (r ^? responseBody . key "recordMap" . key "notion_user" . _Object)
        >>= (listToMaybe . keys)
    of
      Just j  -> return j
      Nothing -> throwError
        "Notion responded not with expected Json, .recordMap.notion_user.{id}"
  return $ unpack userId

cookieOpts :: String -> IO Options
cookieOpts token = do
  now <- getCurrentTime
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
