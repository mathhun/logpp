module PrettyprintSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "sample" $ do
    it "a test" $
      "test" `shouldBe` "test"
