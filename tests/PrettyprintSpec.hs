{-# LANGUAGE OverloadedStrings #-}
module PrettyprintSpec where

import Data.Attoparsec.Text.Parsec
import qualified Data.Map as M
import Data.Text
import Test.Hspec

import LogPP.Prettyprint

log0 :: Text
log0 = "2014-08-13T18:26:34+09:00 TRACE (8): [masterdb](0.00013) SELECT * FROM device WHERE id = :id  ; // bind=>{\"id\":\"sp\"}"
logEntry0 = LogEntry {
    logTime    = "2014-08-13T18:26:34+09:00"
  , logTable   = "masterdb"
  , logText    = "SELECT * FROM device WHERE id = :id"
  , logBinding = fromList [("id","sp")]
}
showLog0 = "2014-08-13T18:26:34+09:00 [masterdb] SELECT * FROM device WHERE id = 'sp'"

p :: Parser Text -> Either String Text
p parser = parseOnly parser log0

spec :: Spec
spec = do
  describe "parseTime" $ do
    it "parses time" $
      p parseTime `shouldBe` Right "2014-08-13T18:26:34+09:00"
  describe "parseTable" $ do
    it "parses table name" $
      parseOnly parseTable "TRACE (8): [masterdb](0.00013) SELECT" `shouldBe` Right "masterdb"
  describe "parseBinding" $ do
    it "parses query bindings" $
      parseOnly parseBinding "// bind=>{\"id\":\"sp\"}" `shouldBe` Right (Right (fromList [("id","sp")]))
  describe "parseLog" $ do
    it "parses a log entry" $
      parseOnly parseLog log0 `shouldBe` Right logEntry0

  describe "showLog" $ do
    it "shows a log entry" $
      showLogEntry logEntry0 `shouldBe` showLog0
  describe "unbind" $ do
    it "replaces placeholders in sql text" $
      unbind "where id = :id" (M.fromList [("id", "sp")]) `shouldBe` "where id = 'sp'"

main :: IO ()
main = hspec spec
