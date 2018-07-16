{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}
-- | Simplification of XML Schema and RelaxNG schema
module Schema where

--import Data.Default
import Data.ByteString.Char8 as BS
import Data.Set as Set

class Default a where
  def :: a

type XMLString = BS.ByteString

newtype ID = ID XMLString
  deriving (Show, Read, Eq, Ord)

data Element = Element {
    minOccurs
  , maxOccurs :: Int
  , name      :: XMLString
  , eType     :: Type
  , targetNamespace :: XMLString
  }

instance Default Element where
  def = Element { minOccurs       = 1
                , maxOccurs       = 1
                , name            = XMLString
                , eType           = def
                , targetNamespace = "" -- inherit
                }

-- | Check that is a simple type.
simpleType :: Type -> Bool
simpleType  = undefined

flatten = undefined

validType :: Type -> Bool
validType  = undefined

data Restriction =
    Enum   [XMLString]
  | Pattern XMLString
  | None -- ^ No restriction expressible here

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
        attrs       :: [Attr]
      , contentType :: [Content]
      }

predefinedTypes = Set.fromList [
    "xs:any"
  , "xs:string"
  , "xs:token"
  , "xs:integer"
  , "xs:date"
  ]
isSimple Ref x
        | x `Set.member` predefinedTypes    = Just True
isSimple Restriction { base }
        | base `Set.member` predefinedTypes = Just True
isSimple Extension {}                       = Just False
isSimple Complex   {}                       = Just False
isSimple _                                  = Nothing -- no idea, need dictionary

instance Default Type where
  def = Ref "xs:any"

data Attr = Attr {
    aName  :: XMLString
  , use   :: Use
  , aType :: Type
  , id    :: Maybe ID
  }

data Use =
    Optional
  | Default XMLString
  | Required

instance Default Use where
  def = Optional

data Content = Seq    [Element]
             | Choice [Element]
             -- no support for xs:all yet

instance Default Content where
  def = Seq []
