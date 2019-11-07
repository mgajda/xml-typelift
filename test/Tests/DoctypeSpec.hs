{-# LANGUAGE OverloadedStrings #-}
module Tests.DoctypeSpec where


import Test.Hspec

import FromXML


spec :: Spec
spec = describe "skipDoctype" $ do
    it "strips initial doctype declaration" $ do
      skipDoctype "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "Hello"
    it "strips doctype after spaces" $ do
      skipDoctype "  \n<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "Hello"
    it "does not strip anything after or inside element" $ do
      let insideElt = "<xml><?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello</xml>"
      skipDoctype  insideElt `shouldBe` insideElt


