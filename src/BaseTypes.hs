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
                ,isFlatXMLType
                ) where

import           Prelude hiding(lookup)

import qualified Data.ByteString.Char8      as BS
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
              ,"Data.ByteString.Char8 as BS"
              ,"Data.Int(Int64)"
              ,"Data.Time.ISO8601.Duration"
              ,"FromXML"
              ,"Data.Time.Calendar(Day)"
              ,"Data.Time.Clock"
              ,"Xeno.DOM as Xeno"
              ]

isFlatXMLType = (`Set.member` flatXMLTypes)

flatXMLTypes = Set.fromList (map fst baseTranslations) Set.\\ Set.fromList ["", "xs:any"]

baseTranslations :: [(BS.ByteString, BS.ByteString)]
baseTranslations = map addNS
    [("any"            , "Xeno.Node"    )
    ,("string"         , "XMLString"    )
    ,("boolean"        , "Bool"         )
    ,("long"           , "Int64"        ) -- or Int64
    ,("duration"       , "Duration"     ) -- TODO: ISO8601 with minor deviations
                                          -- https://www.w3.org/TR/xmlschema-2/#deviantformats
    ,("gYearMonth"     , "Day"          ) -- TODO: shall parse as month and year!
    ,("gYear"          , "Day"          ) -- TODO: shall parse as Gregorian year!
    ,("gMonth"         , "Day"          ) -- TODO: shall parse as month
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
  where
    addNS (a, b) = ("xs:" <> a, b)

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
