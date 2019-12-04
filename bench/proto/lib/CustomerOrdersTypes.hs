{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CustomerOrdersTypes where


import Control.DeepSeq
import Data.Scientific
import Data.Time.LocalTime(ZonedTime)
import FromXML
import GHC.Generics


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

type TopLevel = Root

