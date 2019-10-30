module CliParser
  ( Args(..)
  , parseArgs
  )
where

import           Options.Applicative

data Args = Args { notionId :: String, parentPageId :: String, highlightsPath :: String }

parseArgs :: IO Args
parseArgs = execParser opts
 where
  opts = info
    (argsParser <**> helper)
    (fullDesc <> progDesc "Upload highlights to notion" <> header
      "notionfy - sync all your kindle highlights to notion"
    )


argsParser :: Parser Args
argsParser =
  Args
    <$> strOption
          (long "notionId" <> short 'n' <> help
            "Your notion id, found in the token_v2 cookie"
          )
    <*> strOption
          (long "parentPageId" <> short 'p' <> help
            "Id of the page to which the highlights should be added"
          )
    <*> strOption (long "kindlePath" <> short 'h' <> help "Path to your kindle")

