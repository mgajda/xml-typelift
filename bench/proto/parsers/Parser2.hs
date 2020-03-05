{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Parser2 (parseMethod2) where


import qualified Control.Monad.Fail as F
import Control.Applicative
import Control.DeepSeq
import Data.ByteString (ByteString)
import Data.Functor.Identity
import Data.Scientific
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import GHC.Generics
import GHC.TypeLits
import qualified Data.ByteString.Char8 as BSC

import ParserXP
import SAX


type XMLString = ByteString


data AddressType
    = AddressType {name :: Maybe XMLString,
                   address :: XMLString,
                   city :: XMLString,
                   region :: XMLString,
                   postalCode :: XMLString,
                   country :: XMLString} deriving (Show, Generic, NFData)


data CustomerType
    = CustomerType {name :: Maybe XMLString,
                    companyName :: XMLString,
                    contactName :: XMLString,
                    contactTitle :: XMLString,
                    phone :: XMLString,
                    fax :: Maybe XMLString,
                    fullAddress :: AddressType} deriving (Show, Generic, NFData)


data OrderType
    = OrderType {customerID :: XMLString,
                 employeeID :: XMLString,
                 orderDate :: ZonedTime,
                 requiredDate :: ZonedTime,
                 shipInfo :: ShipInfoType} deriving (Show, Generic, NFData)


data ShipInfoType
    = ShipInfoType {name :: Maybe ZonedTime,
                    shipVia :: Integer,
                    freight :: Scientific,
                    shipName :: XMLString,
                    shipAddress :: XMLString,
                    shipCity :: XMLString,
                    shipRegion :: XMLString,
                    shipPostalCode :: XMLString,
                    shipCountry :: XMLString} deriving (Show, Generic, NFData)

data Customers = Customers {customer :: [CustomerType]} deriving (Show, Generic, NFData)

data Orders = Orders {order :: [OrderType]} deriving (Show, Generic, NFData)

data Root = Root {customers :: Customers, orders :: Orders} deriving (Show, Generic, NFData)

class Located (l :: [Symbol]) where
    type Place l
    parseHere :: Parser l m (Place l)

instance Located '[] where
    type Place '[] = Root
    parseHere = xnode @ "Root" parseHere

instance Located '["Root"] where
    type Place '["Root"] = Root
    parseHere = Root
        <$> xnode @ "Customers" parseHere
        <*> xnode @ "Orders" parseHere

instance Located '["Customers", "Root"] where
    type Place '["Customers", "Root"] = Customers
    parseHere = Customers <$> many (xnode @ "Customer" parseHere)

instance Located '["Orders", "Root"] where
    type Place '["Orders", "Root"] = Orders
    parseHere = Orders <$> many (xnode @ "Order" parseHere)


instance Located '["Customer", "Customers", "Root"] where
    type Place '["Customer", "Customers", "Root"] = CustomerType
    parseHere = CustomerType
        <$> return Nothing
        <*> xnode @ "CompanyName" text
        <*> xnode @ "ContactName" text
        <*> xnode @ "ContactTitle" text
        <*> xnode @ "Phone" text
        <*> optional (xnode @ "Fax" text)
        <*> xnode @ "FullAddress" parseHere

instance Located '["FullAddress", "Customer", "Customers", "Root"] where
    type Place   '["FullAddress", "Customer", "Customers", "Root"] = AddressType
    parseHere = AddressType
        <$> return Nothing
        <*> xnode @ "Address" text
        <*> xnode @ "City" text
        <*> xnode @ "Region" text
        <*> xnode @ "PostalCode" text
        <*> xnode @ "Country" text

zonedTimeStrM :: (F.MonadFail m, Monad m) => ByteString -> m ZonedTime
zonedTimeStrM = parseTimeM True defaultTimeLocale fmt . BSC.unpack
  where
    fmt = iso8601DateFormat (Just "%H:%M:%S")


instance Located '["Order", "Orders", "Root"] where
    type Place '["Order", "Orders", "Root"] = OrderType
    parseHere = OrderType
        <$> xnode @ "CustomerID" text
        <*> xnode @ "EmployeeID" text
        <*> xnode @ "OrderDate" (text >>= zonedTimeStrM)
        <*> xnode @ "RequiredDate" (text >>= zonedTimeStrM)
        <*> xnode @ "ShipInfo" parseHere


instance Located '["ShipInfo", "Order", "Orders", "Root"] where
    type Place '["ShipInfo", "Order", "Orders", "Root"] = ShipInfoType
    parseHere = ShipInfoType
        <$> return Nothing
        <*> xnode @ "ShipVia" (read <$> string)
        <*> xnode @ "Freight" (read <$> string)
        <*> xnode @ "ShipName" text
        <*> xnode @ "ShipAddress" text
        <*> xnode @ "ShipCity" text
        <*> xnode @ "ShipRegion" text
        <*> xnode @ "ShipPostalCode" text
        <*> xnode @ "ShipCountry" text


root :: Parser '[] m (Place '[])
root = parseHere


tryRaw :: (Monad m) => Parser '[] m a -> ByteString -> m (Either String a)
tryRaw parser file = runParser file parser


-- parser2 :: ByteString -> IO (Either String Root)
-- parser2 = tryRaw root


parser2' :: ByteString -> Either String Root
parser2' = runIdentity . tryRaw root

parseMethod2 :: ByteString -> Either String Root
parseMethod2 = parser2'

