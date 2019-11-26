{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -funbox-strict-fields   #-}
-- | Simplification of XML Schema and RelaxNG schema
module Schema where

import Prelude hiding(id)
import Control.DeepSeq
--import Data.ByteString.Char8 as BS
--import Data.Set as Set
import Data.Map
import Data.Data
import GHC.Generics
import Data.Generics.Uniplate.Data()

import FromXML(XMLString)

class Default a where
  def :: a

-- | Top level XML Schema
data Schema = Schema {
    types     :: !(Map XMLString Type) -- ^ Types defined by name
  , tops      :: ![Element]            -- ^ Possible top level elements
  , namespace :: !XMLString
  }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Schema where
  def = Schema Data.Map.empty [] ""

newtype ID = ID XMLString
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

data MaxOccurs = Unbounded | MaxOccurs Int
  deriving (Eq, Ord, Generic, NFData, Data, Typeable, Show, Read)


data Element = Element {
    minOccurs       :: !Int
  , maxOccurs       :: !MaxOccurs
  , eName           :: !XMLString
  , eType           :: !Type
  , targetNamespace :: !XMLString
  }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Element where
  def = Element { eName           = ""
                , minOccurs       =           1
                , maxOccurs       = MaxOccurs 1
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
    Enum    ![XMLString]
  | Pattern   XMLString
  | None -- ^ No restriction expressible here
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Restriction where
  def = None

data Type =
    Ref XMLString
  | Restriction {
        base       :: !XMLString
      , restricted :: !Restriction
      }
  | Extension {
        base  :: !XMLString
      , mixin :: !Type
      } -- ^ Extension of complexType
  | Complex {
        mixed :: !Bool
      , attrs :: ![Attr]
      , inner :: !TyPart
      }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Type where
  -- standard declares that element without type has xs:any
  def = Ref "xs:any"

data Attr = Attr {
    aName :: !XMLString
  , use   :: !Use
  , aType :: !Type
  , id    :: !(Maybe ID)
  }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Attr where
  def = Attr { aName = ""
             , use   = Optional
             , aType = Ref "xs:string"
             , id    = Nothing
             }

data Use =
    Optional
  | Default XMLString
  | Required
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Use where
  def = Optional

data TyPart = Seq    [TyPart]
            | Choice [TyPart]
            | All    [TyPart]
            | Elt    Element
             -- no support for xs:all yet
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default TyPart where
  def = Seq []

{-
instance NFData Attr where
  rnf (Attr a u t i) = rnf a $ rnf u $ rnf t $ rnf i
instance NFData Content
instance NFData Element
instance NFData ID
instance NFData MaxOccurs
instance NFData Restriction
instance NFData Schema
instance NFData Type
instance NFData Use
 -}

