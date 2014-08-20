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
import System.Console.ANSI

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
    Left err -> error $ "parseLog: " ++ err

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
  t <- takeText <* skipSpace
  case (eitherDecodeStrict $ encodeUtf8 t) of
    Right decoded -> return $ Right $ Binding decoded
    Left err -> return $ Left ("parseBinding: " ++ err)

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

showLogEntryColor :: LogEntry -> String
showLogEntryColor e = color e'
  where e' = showLogEntry e

color :: String -> String
color = unwords . reverse . colorTable . reverse . words
  where
    colorTable (w:[]) = [w]
    colorTable (w:ws:[]) = w : ws : []
    colorTable (w1:w2:ws) | w2=="from" || w2=="FROM" = ("\x1b[32m"++w1++"\x1b[0m") : w2 : colorTable ws
                          | otherwise                = w1 : colorTable (w2:ws)

--
-- Command line interface
--

pp :: String -> String
pp = unlines . map prettyprint . T.lines . T.pack

prettyprint :: Text -> String
prettyprint t = case parseOnly parseLog t of
                  Right e -> showLogEntryColor e
                  Left err -> "*** ERROR: " ++ err
