{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Translating base types
--   Checking if a given type is
--   predefined Haskell type,
module BaseTypes(baseTranslations
                ,basePrologue
                ,isSimple
                ,reservedWords
                ,isBaseHaskellType
                ) where

import           Prelude hiding(lookup)

import qualified Data.ByteString.Char8      as BS
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import           Data.String

import           FromXML
import           Schema

-- | Module prologue to import all standard types
basePrologue :: (IsString a, Monoid a) => a
basePrologue  = mconcat $ map makeImport modules
  where
    makeImport modPath = "import " <> modPath <> "\n"
    modules = ["Data.Time.LocalTime(ZonedTime)"
              ,"Data.ByteString.Char8"
              ,"FromXML"
              ,"Data.Time.Calendar(Day)"
              ,"Data.Time.Clock"
              ,"Xeno.DOM as Xeno"
              ]

baseTranslations :: [(BS.ByteString, BS.ByteString)]
baseTranslations =
  [("any"            , "Xeno.Node"    )
  ,("string"         , "XMLString"    )
  ,("boolean"        , "Bool"         )
  ,("hexBinary"      , "BS.ByteString") -- TODO: add hex decoding
  ,("base64Binary"   , "BS.ByteString") -- TODO: add hex decoding
  ,("anyURI"         , "BS.ByteString") -- TODO: add hex decoding
  ,("token"          , "XMLString"    )
  ,("integer"        , "Integer"      ) -- or Integer
  ,("int"            , "Int"          ) -- or Integer
  ,("positiveInteger", "Integer"      ) -- or Integer
  ,("float"          , "Float"        )
  ,("date"           , "Day"          )
  ,("time"           , "DiffTime"     )
  ,("dateTime"       , "ZonedTime"    )
  ,("decimal"        , "Int"          )
  ,("double"         , "Double"       )
  ,("QName"          , "XMLString"    ) -- TODO: split namespace from QNames
  ,("NOTATION"       , "XMLString"    ) -- TODO: we ignore <xs:notation> definitions?
  ,(""               , "TopLevel"     ) -- Document toplevel for the parser
  ]

-- | Check if builder makes Haskell base type
isBaseHaskellType :: XMLString -> Bool
isBaseHaskellType = (`Set.member` baseHaskellTypes)

-- | List of predefined Haskell types that we use.
baseHaskellTypes :: Set.Set XMLString
baseHaskellTypes  = Set.fromList $ usedBases <> otherBases
  where
    usedBases  = map snd baseTranslations
    otherBases = ["String"
                 ,"Integer"
                 ,"Ordering"
                 ,"Maybe"
                 ,"Array"
                 ,"IORef"
                 ,"IO"
                 ,"Monad"
                 ,"()"
                 ,"Void"
                 ,"Ord"
                 ,"Eq"
                 ,"Enum"
                 ,"Bounded"
                 ,"Ordering"
                 ,"Num"
                 ,"RealFrac"
                 ,"Floating"
                 ]

reservedWords :: [XMLString]
reservedWords  = ["do"
                 ,"module"
                 ,"case", "of"
                 ,"if", "then", "else"
                 ,"as"
                 ]

predefinedTypes :: Set.Set XMLString
predefinedTypes = Set.fromList $ map fst baseTranslations

isSimple :: Type -> Maybe Bool
isSimple (Ref x)
        | x    `Set.member` predefinedTypes = Just True
isSimple Restriction { base }
        | base `Set.member` predefinedTypes = Just True -- not always!!!
isSimple  Extension {}                      = Just False
isSimple  Complex   {}                      = Just False
isSimple  _                                 = Nothing -- no idea, need dictionary
