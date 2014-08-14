{-# LANGUAGE OverloadedStrings #-}
module LogPP.Prettyprint (
    Parser(..)
  , parseOnly
  , pp
  , ppT
  , parseTime
) where

--import Data.Attoparsec.Text
import Data.Attoparsec.Text.Parsec
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

data LogEntry = LogEntry
  { logTime  :: Text
  , logTable :: Text
  , logText  :: Text
  , logBinding :: Map Text Text
  }

pp :: String -> String
pp = undefined

ppT :: Text -> Text
ppT = undefined

parseLog :: Parser LogEntry
parseLog = do
  time <- parseTime
  -- skip log level
  table <- undefined
  -- skip elapsed
  text <- undefined
  -- semicolon
  binding <- parseBinding
  return $ LogEntry time table text binding

parseTime :: Parser Text
parseTime = do
  y <- count 4 digit
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  char 'T'
  h <- count 2 digit
  char ':'
  mi <- count 2 digit
  char ':'
  s <- count 2 digit
  return $ T.pack $ concat [y, "-", m, "-", d, " ", h, ":", mi, ":", s]

parseBinding :: Parser (Map Text Text)
parseBinding = undefined
