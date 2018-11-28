{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Test.Hspec
--import Test.QuickCheck

import FromXML

main = hspec $ do
  describe "skipDoctype" $ do
    it "strips initial doctype declaration" $ do
      skipDoctype "<?xml version=\"1.0\" encoding=\"UTF-8\"?>Hello" `shouldBe` "Hello"


