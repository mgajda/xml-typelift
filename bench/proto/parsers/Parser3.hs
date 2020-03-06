{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Parser3 (parseMethod3, parseToArray) where


import Control.DeepSeq
import Control.Monad
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(PS))
import Data.Functor.Identity
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import GHC.Generics
import GHC.Stack
import qualified Control.Monad.Fail as F
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

import Xeno.ChainedSAX as Xeno

import CustomerOrdersTypes


-- | Some XML nodes.
data TopLevelNode = TopLevelNode !ByteString !(UV.Vector Int) deriving (Generic, NFData)

substring :: ByteString -> Int -> Int -> ByteString
substring s' start len = BSU.unsafeTake len (BSU.unsafeDrop start s')
{-# INLINE substring #-}

instance F.MonadFail Identity where fail = error

zonedTimeStr :: ByteString -> ZonedTime
zonedTimeStr = runIdentity . parseTimeM True defaultTimeLocale fmt . BSC.unpack
  where
    fmt = iso8601DateFormat (Just "%H:%M:%S")
{-# INLINE zonedTimeStr #-}

extractTopLevel :: TopLevelNode -> TopLevel
extractTopLevel (TopLevelNode bs arr) =
    Root extractCustomers extractOrders
  where
    customerSize = 22
    orderSize = 26
    extractCustomers =
        let count = arr `UV.unsafeIndex` 0
        in Customers $ map extractCustomer [1,(1 + customerSize)..(1 + customerSize * (count - 1))]
    extractOrders =
        let ofsOrdersCount = arr `UV.unsafeIndex` 0
            ofsOrders = 1 + ofsOrdersCount * customerSize
            ofsFirstOrder = ofsOrders + 1
            count = arr `UV.unsafeIndex` ofsOrders
            offsets = [ofsFirstOrder,(ofsFirstOrder + orderSize)..(ofsFirstOrder + orderSize * (count - 1))]
        in Orders $ map extractOrder offsets
    extractCustomer ofs = CustomerType
        { name = Nothing
        , companyName = extractXmlString ofs
        , contactName = extractXmlString (ofs + 2)
        , contactTitle = extractXmlString (ofs + 4)
        , phone = extractXmlString (ofs + 6)
        , fax = extractMaybeXmlString (ofs + 8)
        , fullAddress = extractFullAddress (ofs + 10) }
    extractXmlString ofs = substring bs bsofs bslen
        where bsofs = arr `UV.unsafeIndex` ofs
              bslen = arr `UV.unsafeIndex` (ofs + 1)
    extractMaybeXmlString ofs
        | bsofs == 0 = Nothing
        | otherwise  = Just $ substring bs bsofs bslen
        where bsofs = arr `UV.unsafeIndex` ofs
              bslen = arr `UV.unsafeIndex` (ofs + 1)
    extractFullAddress ofs = AddressType
        { name = Nothing
        , address = extractXmlString ofs
        , city = extractXmlString (ofs + 2)
        , region = extractXmlString (ofs + 4)
        , postalCode = extractXmlString (ofs + 6)
        , country = extractXmlString (ofs + 8) }
    extractOrder ofs = OrderType
        { customerID = extractXmlString ofs
        , employeeID = extractXmlString (ofs + 2)
        , orderDate = zonedTimeStr $ extractXmlString (ofs + 4)
        , requiredDate = zonedTimeStr $ extractXmlString (ofs + 6)
        , shipInfo = extractShipInfo (ofs + 8) }
    extractShipInfo ofs = ShipInfoType
        { name = Nothing
        , shipVia = read $ BSC.unpack $ extractXmlString ofs
        , freight = read $ BSC.unpack $ extractXmlString (ofs + 2)
        , shipName = extractXmlString (ofs + 4)
        , shipAddress = extractXmlString (ofs + 6)
        , shipCity = extractXmlString (ofs + 8)
        , shipRegion = extractXmlString (ofs + 10)
        , shipPostalCode = extractXmlString (ofs + 12)
        , shipCountry = extractXmlString (ofs + 14)
        }
{-# INLINE extractTopLevel #-}


assertTag :: (HasCallStack, Monad m) => ByteString -> ByteString -> m ()
-- assertTag _ _ = return ()
assertTag actual expected
    | actual == expected = pure ()
    | otherwise          = error $ "Expected tag " ++ show expected ++ ", but got " ++ show actual
{-# INLINE assertTag #-}


parseToArray :: ByteString -> Either String TopLevelNode
parseToArray inputStr = Right $ runST $ do
    vec <- UMV.unsafeNew ((BS.length inputStr `div` 7) * 2) -- minimal tag: <a></a> (7 chars), 2 places per tag
    let p = Xeno.Process {
                      openF = \_ -> pure p
                    , attrF = \_ _ -> pure ()
                    , endOpenF = \_ -> pure ()
                    , textF = \_ -> pure ()
                    , closeF = \_ -> pure p
                    , cdataF = \_ -> pure ()
                    }
        processRoot = p {
                      openF = \tag -> (processCustomers 1) <$ assertTag tag "Root"
                    , closeF = \tag -> p <$ assertTag tag "Root"
                    }
        processCustomers ofs = p {
                      openF = \tag -> do
                        assertTag tag "Customers"
                        UMV.unsafeWrite vec 0 0
                        return $ processCustomer ofs
                    , closeF = \tag -> processOrders ofs <$ assertTag tag "Customers"
                    }
        processCustomer ofs = p {
                      openF =  \tag -> processCompanyName ofs False <$ assertTag tag "Customer"
                    , closeF = \tag -> do
                        assertTag tag "Customer"
                        UMV.unsafeModify vec succ 0 -- TODO if we can get *total numbers* of customers (or offset to last element), we can write this number only once after processing all customers
                        return $ processCustomerOrCustomers ofs
                    }
        processCustomerOrCustomers ofs = p {
                      openF =  openF (processCustomer ofs)
                    , closeF = closeF (processCustomers ofs)
                    }

        processCompanyName = processStrField "CompanyName" processContactName
        processContactName = processStrField "ContactName" processContactTitle
        processContactTitle = processStrField "ContactTitle" processPhone
        processPhone = processStrField "Phone" processFax

        processFax ofs isInsideTag = p {
                      openF = \tag -> if tag == "Fax"
                                      then return $ processFax ofs True
                                      else do
                                          UMV.unsafeWrite vec ofs 0
                                          UMV.unsafeWrite vec (ofs + 1) 0
                                          openF (processFullAddress (ofs + 2) False) tag
                    , textF = \(PS _ start len) ->
                        when isInsideTag $ do
                            UMV.unsafeWrite vec ofs start
                            UMV.unsafeWrite vec (ofs + 1) len
                    , closeF = \tag -> processFullAddress (ofs + 2) False <$ assertTag tag "Fax"
                    }
        processFullAddress ofs _isInsideTag = p {
                      openF  = \tag -> processAddress ofs False <$ assertTag tag "FullAddress"
                    , closeF = \tag -> processCustomer (ofs + 2) <$ assertTag tag "FullAddress"
                    }
        processAddress = processStrField "Address" processCity
        processCity = processStrField "City" processRegion
        processRegion = processStrField "Region" processPostalCode
        processPostalCode = processStrField "PostalCode" processCountry
        processCountry = processStrField "Country" processFullAddress
        --
        -- Orders
        --
        processOrders ofs = p {
                      openF = \tag -> do
                        assertTag tag "Orders"
                        UMV.unsafeWrite vec ofs 0
                        return $ processOrder ofs (ofs + 1)
                    , closeF = \tag -> processRoot <$ assertTag tag "Orders"
                    }
        processOrder ofsCounter ofs = p {
                      openF = \tag -> processCustomerID ofsCounter ofs False <$ assertTag tag "Order"
                    , closeF = \tag -> do
                        assertTag tag "Order"
                        UMV.unsafeModify vec succ ofsCounter
                        return $ processOrderOrOrders ofsCounter ofs
                    }
        processOrderOrOrders ofsCounter ofs = p {
                      openF  = openF (processOrder ofsCounter ofs)
                    , closeF = closeF (processOrders ofs)
                    }

        processCustomerID ofsCounter = processStrField "CustomerID" (processEmployeeID ofsCounter)
        processEmployeeID ofsCounter = processStrField "EmployeeID" (processOrderDate ofsCounter)
        processOrderDate ofsCounter = processStrField "OrderDate" (processRequiredDate ofsCounter)
        processRequiredDate ofsCounter = processStrField "RequiredDate" (processShipInfo ofsCounter)

        processShipInfo ofsCounter ofs _isInsideTag = p {
                      openF  = \tag -> processShipVia ofsCounter ofs False <$ assertTag tag "ShipInfo"
                    , closeF = \tag -> processOrder ofsCounter (ofs + 2) <$ assertTag tag "ShipInfo"
                    }

        processShipVia ofsCounter = processStrField "ShipVia" (processFreight ofsCounter)
        processFreight ofsCounter = processStrField "Freight" (processShipName ofsCounter)
        processShipName ofsCounter = processStrField "ShipName" (processShipAddress ofsCounter)
        processShipAddress ofsCounter = processStrField "ShipAddress" (processShipCity ofsCounter)
        processShipCity ofsCounter = processStrField "ShipCity" (processShipRegion ofsCounter)
        processShipRegion ofsCounter = processStrField "ShipRegion" (processShipPostalCode ofsCounter)
        processShipPostalCode ofsCounter = processStrField "ShipPostalCode" (processShipCountry ofsCounter)
        processShipCountry ofsCounter = processStrField "ShipCountry" (processShipInfo ofsCounter)
        --
        -- Utils
        --
        processStrField tagName next ofs isInsideTag =
            p { openF = \tag -> processStrField tagName next ofs True <$ assertTag tag tagName
              , textF = \(PS _ start len) ->
                  when isInsideTag $ do
                      UMV.unsafeWrite vec ofs start
                      UMV.unsafeWrite vec (ofs + 1) len
              , closeF = \tag -> next (ofs + 2) False <$ assertTag tag tagName
              }
    --
    Xeno.process processRoot inputStr
    arr <- UV.unsafeFreeze vec -- TODO only part of array!
    return $ TopLevelNode inputStr arr
{-# INLINE parseToArray #-}


parseMethod3 :: ByteString -> Either String TopLevel
parseMethod3 = fmap extractTopLevel . parseToArray
{-# INLINE parseMethod3 #-}

