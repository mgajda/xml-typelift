module States where

import Data.ByteString.Char8 as BS
import Data.Set
import GHC.Generics

import Schema


data Builder = BContent Content
             | BElement Element
             | BType    Type
             | BAttr    Attr
  deriving (Eq, Ord, Show, Generic)
