{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simplification of XML Schema and RelaxNG schema
module Schema where

--import Data.Default
import Data.ByteString.Char8 as BS
import Data.Set as Set
import Data.Map
import GHC.Generics

class Default a where
  def :: a

-- | Top level XML Schema
data Schema = Schema {
    types     :: Map XMLString Type -- ^ Types defined by name
  , tops      :: [Element]          -- ^ Possible top level elements
  , namespace :: XMLString
  }
  deriving (Eq, Ord, Show, Generic)

instance Default Schema where
  def = Schema Data.Map.empty [] ""

-- | Type alias to the used `String`-like type
type XMLString = BS.ByteString

newtype ID = ID XMLString
  deriving (Show, Read, Eq, Ord, Generic)

data Element = Element {
    minOccurs
  , maxOccurs :: Int
  , name      :: XMLString
  , eType     :: Type
  , targetNamespace :: XMLString
  }
  deriving (Eq, Ord, Show, Generic)

instance Default Element where
  def = Element { minOccurs       = 1
                , maxOccurs       = 1
                , name            = ""
                , eType           = def
                , targetNamespace = "" -- inherit
                }

-- | Check that is a simple type.
simpleType :: Type -> Bool
simpleType  = undefined

-- | Expand references, extensions, and restrictions
flatten :: Type -> Map XMLString Type -> Type
flatten = undefined

validType :: Type -> Bool
validType  = undefined

data Restriction =
    Enum   [XMLString]
  | Pattern XMLString
  | None -- ^ No restriction expressible here
  deriving (Eq, Ord, Show, Generic)

instance Default Restriction where
  def = None

data Type =
    Ref XMLString
  | Restriction {
        base       :: XMLString
      , restricted :: Restriction
      }
  | Extension {
        base  :: XMLString
      , mixin :: Type
      } -- ^ Extension of complexType
  | Complex {
        attrs :: [Attr]
      , subs  :: Content
      }
  deriving (Eq, Ord, Show, Generic)

predefinedTypes :: Set.Set XMLString
predefinedTypes = Set.fromList [
    "any"
  , "string"
  , "token"
  , "integer"
  , "date"
  ]

isSimple :: Type -> Maybe Bool
isSimple (Ref x)
        | x    `Set.member` predefinedTypes = Just True
isSimple Restriction { base }
        | base `Set.member` predefinedTypes = Just True
isSimple  Extension {}                      = Just False
isSimple  Complex   {}                      = Just False
isSimple  _                                 = Nothing -- no idea, need dictionary

instance Default Type where
  def = Ref "xs:any"

data Attr = Attr {
    aName  :: XMLString
  , use   :: Use
  , aType :: Type
  , id    :: Maybe ID
  }
  deriving (Eq, Ord, Show, Generic)

instance Default Attr where
  def = Attr "" def def Nothing

data Use =
    Optional
  | Default XMLString
  | Required
  deriving (Eq, Ord, Show, Generic)

instance Default Use where
  def = Optional

data Content = Seq    [Element]
             | Choice [Element]
             -- no support for xs:all yet
  deriving (Eq, Ord, Show, Generic)

instance Default Content where
  def = Seq []

-- | Append `Element` to whatever `Content` type we have.
contentAppend :: Content -> Element -> Content
contentAppend (Choice cs) c = Choice (c:cs)
contentAppend (Seq    ss) c = Seq    (c:ss)

