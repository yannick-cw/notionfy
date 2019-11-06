module CliParser
  ( Args(..)
  , parseArgs
  , HasNotion(..)
  )
where

import           Options.Applicative

data Args = Args { notionId :: String, parentPageId :: String, highlightsPath :: String }

class HasNotion r where
  notionConf :: r -> (String, String)

instance HasNotion Args where
  notionConf r = (notionId r, parentPageId r)

parseArgs :: IO Args
parseArgs = execParser opts
 where
  opts = info
    (argsParser <**> helper)
    (fullDesc <> progDesc "Upload your highlights to notion" <> header
      "notionfy - sync all your kindle highlights to notion"
    )


argsParser :: Parser Args
argsParser =
  Args
    <$> strOption
          (  long "token"
          <> short 'n'
          <> help
               "Your notion token, found in the token_v2 cookie when you open notion in the browser"
          )
    <*> strOption
          (  long "page"
          <> short 'p'
          <> help
               "Id of the page to which the highlights should be added, you find it in the url if you open the page in the browser (uuid)"
          )
    <*> strOption
          (long "kindle" <> short 'k' <> help
            "Path to your kindle, e.g. on Mac /Volumes/Kindle"
          )

