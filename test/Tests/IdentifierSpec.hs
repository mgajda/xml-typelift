{-# LANGUAGE OverloadedStrings #-}
module Tests.IdentifierSpec (spec) where


import Test.Hspec

import Identifiers

spec :: Spec
spec = do
  describe "normalizeTypeName" $ do
      it "can process simple names" $ example $ do
          normalizeTypeName "abc" `shouldBe` "Abc"
          normalizeTypeName "def" `shouldBe` "Def"
      it "can split by alphas" $ example $ do
          normalizeTypeName "Abc Def Ghi" `shouldBe` "AbcDefGhi"
      it "can escape first non-alphas" $ example $ do
          normalizeTypeName "123"      `shouldBe` "_123"
          normalizeTypeName "  123"    `shouldBe` "_123"
          normalizeTypeName " 123 456" `shouldBe` "_123456"
          normalizeTypeName " 123 abc" `shouldBe` "_123Abc"
          normalizeTypeName " 123abc"  `shouldBe` "_123abc"
      it "can accept names started with underscore" $ example $ do
          normalizeTypeName "_123"         `shouldBe` "_123"
          normalizeTypeName "_abc"         `shouldBe` "_Abc"
          normalizeTypeName "Abc_Def_Ghi"  `shouldBe` "Abc_Def_Ghi"
          normalizeTypeName "_Abc_Def_Ghi" `shouldBe` "_Abc_Def_Ghi"
  describe "normalizeFieldName" $ do
      it "can escape keywords" $ example $ do
          normalizeFieldName "let"   `shouldNotBe` "let"
          normalizeFieldName "class" `shouldNotBe` "class"
      it "can process complex names" $ example $ do
          normalizeFieldName "abc def ghi" `shouldBe` "abcDefGhi"
          normalizeFieldName "_ Abc def"   `shouldBe` "_abcDef"
          normalizeFieldName "_Abc def"   `shouldBe` "_abcDef"

