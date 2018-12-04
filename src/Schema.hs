{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE ViewPatterns               #-}
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
    types     ::                 Map XMLString Type -- ^ Types defined by name
  , tops      ::                ![Element]          -- ^ Possible top level elements
  , namespace :: {-# UNPACK #-} !XMLString
  }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Schema where
  def = Schema Data.Map.empty [] ""

newtype ID = ID XMLString
  deriving (Show, Read, Eq, Ord, Generic, NFData, Data, Typeable)

newtype MaxOccurs = MaxOccurs Int
  deriving (Eq, Ord, Bounded, Generic, NFData, Data, Typeable)

instance Show MaxOccurs where
  showsPrec _ (isUnbounded -> True) = ("unbounded"++)
  showsPrec p (MaxOccurs m)         = showsPrec p m

instance Read MaxOccurs where
  readsPrec _ ('u':'n':'b':'o':'u':'n':'d':'e':'d':rest) = [(MaxOccurs maxBound,        rest)]
  readsPrec p  x                                         = [(MaxOccurs r       ,        rest)
                                                           |(          r :: Int,        rest) <- readsPrec p x]

data Element = Element {
    minOccurs       :: !Int
  , maxOccurs       :: !MaxOccurs -- `maxint` value means `unbounded`
  , eName           :: !XMLString
  , eType           :: !Type
  , targetNamespace :: !XMLString
  }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

isUnbounded :: MaxOccurs -> Bool
isUnbounded i | i==maxBound = True
isUnbounded _               = False

instance Default Element where
  def = Element { eName           = ""
                , minOccurs       =           1
                , maxOccurs       = MaxOccurs 1 -- Nothing means `unbounded`
                , eType           = def
                , targetNamespace = "" -- inherit
                }

-- | Check that is a simple type.
simpleType :: Type -> Bool
simpleType  = undefined

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
        base       :: {-# UNPACK #-} !XMLString
      , restricted :: {-# UNPACK #-} !Restriction
      }
  | Extension {
        base  :: {-# UNPACK #-} !XMLString
      , mixin :: {-# UNPACK #-} !Type
      } -- ^ Extension of complexType
  | Complex {
        mixed :: {-# UNPACK #-}   Bool
      , attrs :: {-# UNPACK #-} ![Attr]
      , inner :: {-# UNPACK #-} !TyPart
      }
  deriving (Eq, Ord, Show, Generic, NFData, Data, Typeable)

instance Default Type where
  -- standard declares that element without type has xs:any
  def = Ref "xs:any"

data Attr = Attr {
    aName :: {-# UNPACK #-} !XMLString
  , use   :: {-# UNPACK #-} !Use
  , aType :: {-# UNPACK #-} !Type
  , id    ::                 Maybe ID
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

