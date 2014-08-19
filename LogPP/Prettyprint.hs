{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module LogPP.Prettyprint where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Attoparsec.Text.Parsec
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.Generics

--
-- Data Types
--

data LogEntry = LogEntry
  { logTime  :: Text
  , logTable :: Text
  , logText  :: Text
  , logBinding :: Binding
  } deriving (Eq, Show)

data Binding = Binding { binding :: M.Map Text Text } deriving (Show, Generic, Eq)
instance FromJSON Binding
fromList :: [(Text,Text)] -> Binding
fromList = Binding . M.fromList

pp :: String -> String
pp = undefined

ppT :: Text -> Text
ppT = undefined

--
-- Parser
--

parseLog :: Parser LogEntry
parseLog = do
  time <- parseTime
  table <- parseTable
  (text, b) <- parseSql
  case b of
    Right binding -> return $ LogEntry time table text binding
    Left err -> error err

parseTime :: Parser Text
parseTime = takeTill (' ' ==) <* skipSpace

parseTable :: Parser Text
parseTable = string "TRACE (8): [" *> takeTill (']' ==) <* skipWhile (' ' /=)

parseSql :: Parser (Text, Either String Binding)
parseSql = do
  s <- takeTill (== ';') <* (string ";" >> skipSpace)
  b <- parseBinding
  return (T.strip s, b)

parseBinding :: Parser (Either String Binding)
parseBinding = do
  string "// bind=>"
  t <- takeText
  case (eitherDecodeStrict $ encodeUtf8 t) of
    Right decoded -> return $ Right $ Binding decoded
    Left err -> return $ Left err

{-
Data.Aeson
  decode :: Data.ByteString.Lazy.ByteString ->

Data.Text.Encoding
  encodeUtf8 :: Data.Text.Internal -> Data.ByteString.ByteString
-}

--
-- Printer
--

showLogEntry :: LogEntry -> String
showLogEntry e = time ++ " [" ++ tbl ++ "] " ++ sql
  where
    time = T.unpack $ logTime e
    tbl  = T.unpack $ logTable e
    sql  = T.unpack $ unbind (logText e) (binding $ logBinding e)

unbind :: Text -> (M.Map Text Text) -> Text
unbind t b = T.unwords $ map unbind1 $ T.words t
  where
    unbind1 w | T.isPrefixOf ":" w = replace1 (T.drop 1 w)
              | otherwise = w
    replace1 w = case M.lookup w b of
                   Just t -> T.concat ["'", t, "'"]
                   Nothing -> w

