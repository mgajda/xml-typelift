{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Parser6 (parseMethod6, parseMethod6OnlyEvents) where


import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Void
import Text.Megaparsec hiding (region)
import qualified Data.ByteString.Char8 as BSC

import Xeno.StreamedSAX as Xeno

import CustomerOrdersTypes
import Utils


instance Stream [SAXEvent] where
    type Token [SAXEvent] = SAXEvent
    type Tokens [SAXEvent] = [SAXEvent]
    take1_ []     = Nothing
    take1_ (s:sx) = Just (s, sx)
    takeN_ n sx | n <= 0    = Just ([], sx)
                | null sx   = Nothing -- NB: this is requirement of `takeN_` and it *must* be after first case
                | otherwise = Just (take n sx, drop n sx)
    tokensToChunk _ sx = sx
    chunkToTokens _ sx = sx
    reachOffset _ p = ("<line>", p)
    showTokens _ toks = show toks
    chunkLength _ toks = length toks
    takeWhile_ p s = span p s


type SaxParser a = Parsec Void [SAXEvent] a


parseRoot :: SaxParser Root
parseRoot = do
    skipText
    withTag "Root" $ do
        customers <- parseCustomers
        orders    <- parseOrders
        return $ Root customers orders


parseCustomers :: SaxParser Customers
parseCustomers = withTag "Customers" $ do
    customers <- many parseCustomer
    return $ Customers customers


parseCustomer :: SaxParser CustomerType
parseCustomer = withTagAndAttrs "Customer" $ \_ -> do
    let name = Nothing
    companyName  <- tagValue "CompanyName"
    contactName  <- tagValue "ContactName"
    contactTitle <- tagValue "ContactTitle"
    phone        <- tagValue "Phone"
    fax          <- optional $ tagValue "Fax"
    fullAddress  <- parseFullAddress
    return $ CustomerType {..}


parseFullAddress :: SaxParser AddressType
parseFullAddress = withTag "FullAddress" $ do
    let name = Nothing
    address    <- tagValue "Address"
    city       <- tagValue "City"
    region     <- tagValue "Region"
    postalCode <- tagValue "PostalCode"
    country    <- tagValue "Country"
    return $ AddressType {..}


parseOrders :: SaxParser Orders
parseOrders = withTag "Orders" $ do
    customers <- many parseOrder
    return $ Orders customers


parseOrder :: SaxParser OrderType
parseOrder = withTag "Order" $ do
    customerID   <- tagValue "CustomerID"
    employeeID   <- tagValue "EmployeeID"
    orderDate    <- (tagValue "OrderDate" >>= zonedTimeStrM)
    requiredDate <- (tagValue "RequiredDate" >>= zonedTimeStrM)
    shipInfo     <- parseShipInfo
    return $ OrderType {..}


parseShipInfo :: SaxParser ShipInfoType
parseShipInfo = withTagAndAttrs "ShipInfo" $ \_ -> do
    let name = Nothing
    shipVia        <- (read . BSC.unpack) <$> tagValue "ShipVia"
    freight        <- (read . BSC.unpack) <$> tagValue "Freight"
    shipName       <- tagValue "ShipName"
    shipAddress    <- tagValue "ShipAddress"
    shipCity       <- tagValue "ShipCity"
    shipRegion     <- tagValue "ShipRegion"
    shipPostalCode <- tagValue "ShipPostalCode"
    shipCountry    <- tagValue "ShipCountry"
    return $ ShipInfoType {..}


parseMethod6 :: ByteString -> Either String TopLevel
parseMethod6 = first errorBundlePretty
             . parse parseRoot "str"
             . Xeno.process


parseMethod6OnlyEvents :: ByteString -> [SAXEvent]
parseMethod6OnlyEvents = Xeno.process

-- Utilities:


openTag :: (MonadParsec e [SAXEvent] m)
        => ByteString
        -> m ()
openTag tagName = do
    void $ satisfy $ \case
        OpenElt tagName' -> tagName == tagName'
        _                -> False
    void $ satisfy $ \case
        EndOpenElt tagName' -> tagName == tagName'
        _                   -> False
    skipText


openTagAttrs :: (MonadParsec e [SAXEvent] m)
        => ByteString
        -> m [(ByteString, ByteString)]
openTagAttrs tagName = do
    void $ satisfy $ \case
        OpenElt tagName' -> tagName == tagName'
        _                -> False
    attrs <- (map (\(AttrElt attr val) -> (attr,val))) `fmap` (many $ satisfy $ \case
            AttrElt _ _ -> True
            _           -> False)
    void $ satisfy $ \case
        EndOpenElt tagName' -> tagName == tagName'
        _                   -> False
    skipText
    return attrs


tagValue :: (MonadParsec e [SAXEvent] m)
         => ByteString
         -> m ByteString
tagValue tagName = do
    void $ satisfy $ \case
        OpenElt tagName' -> tagName == tagName'
        _                -> False
    void $ satisfy $ \case
        EndOpenElt tagName' -> tagName == tagName'
        _                   -> False
    (TextElt txt) <- satisfy $ \case
        TextElt _ -> True
        _         -> False
    void $ satisfy $ \case
        CloseElt tagName' -> tagName == tagName'
        _                 -> False
    skipText
    return txt


skipText :: (MonadParsec e [SAXEvent] m)
         => m ()
skipText = void $ many $ satisfy $ \case
    TextElt _ -> True
    _         -> False


closeTag :: (MonadParsec e [SAXEvent] m)
         => ByteString
         -> m ()
closeTag tagName = do
    void $ satisfy $ \case
        CloseElt tagName' -> tagName == tagName'
        _                 -> False
    skipText


withTag :: ByteString -> SaxParser a -> SaxParser a
withTag tagName sp = do
    openTag tagName
    res <- sp
    closeTag tagName
    return res


withTagAndAttrs :: ByteString -> ([(ByteString, ByteString)] -> SaxParser a) -> SaxParser a
withTagAndAttrs tagName sp = do
    attrs <- openTagAttrs tagName
    res <- sp attrs
    closeTag tagName
    return res
