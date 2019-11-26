{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TupleSections        #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ToXmlInstances where


import Data.ByteString (ByteString)
import Data.Maybe
import Data.Time.LocalTime
import qualified Data.ByteString.Char8 as BC

import CustomerOrdersTypes
import ToXML


zonedTimeToBs :: ZonedTime -> ByteString
zonedTimeToBs ZonedTime{..} = BC.pack $ (map (\c -> if c == ' ' then 'T' else c)) $ show $ zonedTimeToLocalTime


instance ToXML ShipInfoType where
    toXML' ii ShipInfoType{..} =
        taga ii "ShipInfo" (maybeToList $ (("ShippedDate",) . zonedTimeToBs) <$> name) $ mconcat
            [ tags nii "ShipVia" (BC.pack $ show $ shipVia)
            , tags nii "Freight" (BC.pack $ show $ freight)
            , tags nii "ShipName" shipName
            , tags nii "ShipAddress" shipAddress
            , tags nii "ShipCity" shipCity
            , tags nii "ShipRegion" shipRegion
            , tags nii "ShipPostalCode" shipPostalCode
            , tags nii "ShipCountry" shipCountry
            ]
      where
        nii = nextIndent ii
    {-# INLINE toXML' #-}


instance ToXML OrderType where
    toXML' ii OrderType{..} =
        tag ii "Order" $ mconcat
            [ tags nii "CustomerID"   customerID
            , tags nii "EmployeeID"   employeeID
            , tags nii "OrderDate"    (zonedTimeToBs orderDate)
            , tags nii "RequiredDate" (zonedTimeToBs requiredDate)
            , toXML' nii shipInfo
            ]
      where
        nii = nextIndent ii
    {-# INLINE toXML' #-}


instance ToXML AddressType where
    toXML' ii AddressType{..} =
        tag ii "FullAddress" $ mconcat
            [ tags nii "Address" address
            , tags nii "City" city
            , tags nii "Region" region
            , tags nii "PostalCode" postalCode
            , tags nii "Country" country
            ]
      where
        nii = nextIndent ii
    {-# INLINE toXML' #-}


instance ToXML CustomerType where
    toXML' ii CustomerType{..} =
        taga ii "Customer" (maybeToList $ ("CustomerID",) <$> name) $ mconcat $
            [ tags nii "CompanyName" companyName
            , tags nii "ContactName" contactName
            , tags nii "ContactTitle" contactTitle
            , tags nii "Phone" phone
            ] ++
            (maybeToList $ (tags nii "Fax") <$> fax) ++
            [ toXML' nii fullAddress ]
      where
        nii = nextIndent ii
    {-# INLINE toXML' #-}


-- instance ToXML [AddressType] where
    -- toXML' ii lst = tag ii "Addresses" $ mconcat $ map (toXML' (nextIndent ii)) lst
    -- {-# INLINE toXML' #-}


instance ToXML Customers where
    toXML' ii (Customers lst) = tag ii "Customers" $ mconcat $ map (toXML' (nextIndent ii)) lst
    {-# INLINE toXML' #-}


instance ToXML Orders where
    toXML' ii (Orders lst) = tag ii "Orders" $ mconcat $ map (toXML' (nextIndent ii)) lst
    {-# INLINE toXML' #-}


instance ToXML Root where
    toXML' ii Root{..} = tag ii "Root" $ mconcat [ toXML' nii customers, toXML' nii orders ]
      where
        nii = nextIndent ii
    {-# INLINE toXML' #-}
