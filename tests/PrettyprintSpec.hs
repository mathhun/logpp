{-# LANGUAGE OverloadedStrings #-}
module PrettyprintSpec where

import Data.Text
import Test.Hspec

import LogPP.Prettyprint

log0 :: Text
log0 = "2014-08-13T18:26:34+09:00 TRACE (8): [dm_common_master](0.00013) SELECT * FROM device WHERE id = :id  ; // bind=>{\":id\":\"sp\"}"

p :: Parser Text -> Either String Text
p parser = parseOnly parser log0

spec :: Spec
spec = do
  describe "parseTime" $ do
    it "parses time" $
      p parseTime `shouldBe` Right "2014-08-13 18:26:34"

main :: IO ()
main = hspec spec
