{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_GHC -fno-warn-orphans -Wno-name-shadowing #-}
module Parser1 (parseMethod1) where


import Data.ByteString (ByteString)
import FromXML
import Xeno.DOM as Xeno
import qualified Data.ByteString.Char8 as BSC

import CustomerOrdersTypes
import Utils


instance FromXML Root where
    fromXML node = do
        ensure node Xeno.name "Root"
        fromXML' node
    fromXML' node = do
        let childs@(customerElms:orderElms:_) = children node
        ensureCount childs 2
        customers <- fromXML customerElms
        orders <- fromXML orderElms
        return Root {..}


instance FromXML Customers where
    fromXML node = do
        ensure node Xeno.name "Customers"
        fromXML' node
    fromXML' node =
        Customers <$> mapM fromXML (children node)


instance FromXML CustomerType where
    fromXML node = do
        ensure node Xeno.name "Customer"
        fromXML' node
    fromXML' node = do
        (companyName, (contactName, (contactTitle, (phone, (fax, (fullAddress, _)))))) <- (
            fromXmlObligatory "CompanyName" allTextM $
            fromXmlObligatory "ContactName" allTextM $
            fromXmlObligatory "ContactTitle" allTextM $
            fromXmlObligatory "Phone" allTextM $
            fromXmlMaybe "Fax" allTextM $
            fromXmlObligatory "FullAddress" fromXML' $
            endOfFields
            )
            (children node)
        let name = Nothing
        return CustomerType { .. }


instance FromXML AddressType where
    fromXML node = do
        ensure node Xeno.name "FullAddress"
        fromXML' node
    fromXML' node = do
        (address, (city, (region, (postalCode, (country, _))))) <- (
            fromXmlObligatory "Address" allTextM $
            fromXmlObligatory "City" allTextM $
            fromXmlObligatory "Region" allTextM $
            fromXmlObligatory "PostalCode" allTextM $
            fromXmlObligatory "Country" allTextM $
            endOfFields
            )
            (children node)
        let name = Nothing
        return $ AddressType { .. }


instance FromXML Orders where
    fromXML node = do
        ensure node Xeno.name "Orders"
        fromXML' node
    fromXML' node =
        Orders <$> mapM fromXML (children node)


instance FromXML OrderType where
    fromXML node = do
        ensure node Xeno.name "Order"
        fromXML' node
    fromXML' node = do
        (customerID, (employeeID, (orderDate, (requiredDate, (shipInfo, _))))) <- (
            fromXmlObligatory "CustomerID" allTextM $
            fromXmlObligatory "EmployeeID" allTextM $
            fromXmlObligatory "OrderDate" zonedTimeM $
            fromXmlObligatory "RequiredDate" zonedTimeM $
            fromXmlObligatory "ShipInfo" fromXML' $
            endOfFields
            )
            (children node)
        return $ OrderType { .. }


instance FromXML ShipInfoType where
    fromXML node = do
        ensure node Xeno.name "ShipInfo"
        fromXML node
    fromXML' node = do
        (shipVia, (freight, (shipName, (shipAddress, (shipCity, (shipRegion, (shipPostalCode, (shipCountry, _)))))))) <- (
            fromXmlObligatory "ShipVia" readM $
            fromXmlObligatory "Freight" readM $
            fromXmlObligatory "ShipName" allTextM $
            fromXmlObligatory "ShipAddress" allTextM $
            fromXmlObligatory "ShipCity" allTextM $
            fromXmlObligatory "ShipRegion" allTextM $
            fromXmlObligatory "ShipPostalCode" allTextM $
            fromXmlObligatory "ShipCountry" allTextM $
            endOfFields
            )
            (children node)
        let name = Nothing
        return $ ShipInfoType { .. }


-- Utilities 


ensure :: Node -> (Node -> ByteString) -> ByteString -> Result ()
ensure node fieldReader expected
    | field == expected = return ()
    | otherwise         = fail $ "Expected '" ++ BSC.unpack expected ++ "', but got '" ++ BSC.unpack field ++ "'"
  where
    field = fieldReader node


ensureCount :: [a] -> Int -> Result ()
ensureCount lst expectedLen
    | lstLen == expectedLen = return ()
    | otherwise             = fail $ "Wrong list length, expected " ++ show expectedLen ++ ", got " ++ show lstLen
  where
    lstLen = length lst


fromXmlObligatory :: ByteString -> (Node -> Result a) -> ([Node] -> Result b) -> [Node] -> Result (a, b)
fromXmlObligatory _ _ _ [] = error "TODO" -- TODO
fromXmlObligatory expectedName processing next (node:cts) = do
    ensure node Xeno.name expectedName
    res1 <- processing node
    res2 <- next cts
    return (res1, res2)


fromXmlMaybe :: ByteString -> (Node -> Result a) -> ([Node] -> Result b) -> [Node] -> Result (Maybe a, b)
fromXmlMaybe _ _ _ [] = error "TODO" -- TODO
fromXmlMaybe expectedName processing next nodes@(node:cts)
    | Xeno.name node == expectedName =
        (,) <$> (Just <$> processing node) <*> next cts
    | otherwise =
        (Nothing,) <$> next nodes


endOfFields :: ([Node] -> Result ())
endOfFields _ = return ()


parseMethod1 :: ByteString -> Result TopLevel
-- parseMethod1 = ((\(Right xml) -> fromXML xml ::Result TopLevel) . parse)
parseMethod1 bs =
    case parse bs of
      Right xml -> fromXML xml
      Left err -> error (show err)


