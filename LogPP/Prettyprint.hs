{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LogPP.Prettyprint (
    Parser(..)
  , parseOnly
  , pp
  , ppT
  , parseTime
  , parseTable
) where

import Control.Applicative
import Data.Aeson (FromJSON, decode)
--import Data.Attoparsec.Text
import Data.Attoparsec.Text.Parsec
import qualified Data.Attoparsec.ByteString as AB
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics (Generic)

data LogEntry = LogEntry
  { logTime  :: Text
  , logTable :: Text
  , logText  :: Text
  , logBinding :: Maybe Binding
  }

data Binding = Binding { binding :: Map Text Text } deriving (Show, Generic )
instance FromJSON Binding

pp :: String -> String
pp = undefined

ppT :: Text -> Text
ppT = undefined

parseLog :: Parser LogEntry
parseLog = do
  time <- parseTime
  -- skip log level
  table <- parseTable
  -- skip elapsed
  (text, binding) <- parseSql
  return $ LogEntry time table text binding

parseTime :: Parser Text
parseTime = takeTill (' ' ==)

-- log0 = "2014-08-13T18:26:34+09:00 TRACE (8): [dm_common_master](0.00013) SELECT * FROM device WHERE id = :id  ; // bind=>{\":id\":\"sp\"}"
parseTable :: Parser Text
parseTable = string "TRACE (8): [" *> takeTill (']' ==) <* skipWhile (' ' /=)

parseSql :: Parser (Text, Maybe Binding)
parseSql = do
  s <- takeTill (== ';')
  b <- parseBinding
  return (s, b)

parseBinding :: Parser (Maybe Binding)
parseBinding = do
  t <- takeText
  return $ decode $ encodeUtf8 t
