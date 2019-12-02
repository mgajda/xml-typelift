{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser5 (parseMethod5) where


import Data.ByteString (ByteString)
import Data.IORef
import Xeno.SAX as Xeno
import qualified Data.ByteString.Char8 as BSC

import CustomerOrdersTypes
import Utils


-- Note: standard `Maybe` is lazy, so I think that if
--       I change to strict version:
--
-- data Maybe' a = Nothing' | Just' !a
--
--       parser will work better: no lazy evaluations,
--       no memory chunks.
--
--       But benchmarks shows no difference
--       with standard `Maybe` so I back to standard.


parseMethod5 :: ByteString -> IO TopLevel
parseMethod5 str = do
    -- Current level of parsing
    -- Using this level we distinguish same named tags on different levels
    levelRef <- newIORef (0::Int)
    -- Levels:
    -- level: 0
    -- <Root>
    --   level: 1
    --   <Customers>
    --     level: 2
    --     <Customer>
    --       level: 3
    --       <CompanyName>
    --       <ContactName>
    --       <ContactTitle>
    --       <Phone>
    --       <Fax>
    --       <FullAddress>
    --         level: 4
    --         <Address>
    --         <City>
    --         <Region>
    --         <PostalCode>
    --         <Country>
    --   <Orders>
    --     level: 2
    --     <Order>
    --       level: 3
    --       <CustomerID>
    --       <EmployeeID>
    --       <OrderDate>
    --       <RequiredDate>
    --       <ShipInfo>
    --         level: 4
    --         <ShipVia>
    --         <Freight>
    --         <ShipName>
    --         <ShipAddress>
    --         <ShipCity>
    --         <ShipRegion>
    --         <ShipPostalCode>
    --         <ShipCountry>
    --
    -- Parts of `TopLevel`. We use Ref for each field/inner field
    -- of TopLevel. For example, when we reach some text tag (i.e. <CompanyName>),
    -- first `openF` fired, then `textF` and then `closeF`. So
    -- in `openF` we set "writer" to according ref (`companyNameRef` correspondingly),
    -- and in `textF` we write to this ref. But because of `textF` fired for
    -- between-tag text, we need to clear "writer". We do that in `closeF`.
    --
    -- Code template:
    --
    -- openF = \tagName -> case (level, tagName) of
    --            ...
    --            (3, "CompanyName") -> writeTextToRef companyNameRef
    --            ...
    -- textF = \txt ->
    --            readIORef textValueWriterRef >>= \writer -> writer txt
    --            ...
    -- closeF = \tagName -> case (level, tagName) of
    --            ...
    --            -- Clear writer:
    --            (3, _) -> writeIORef textValueWriterRef (\_ -> return ())
    --            ...
    --
    rootRef            <- newIORef Nothing
    -- Customers:
    customersRef       <- newIORef Nothing
    customersListRef   <- newIORef []
    companyNameRef     <- newIORef Nothing
    contactNameRef     <- newIORef Nothing
    contactTitleRef    <- newIORef Nothing
    phoneRef           <- newIORef Nothing
    faxRef             <- newIORef Nothing
    fullAddressRef     <- newIORef Nothing
    addressRef         <- newIORef Nothing
    cityRef            <- newIORef Nothing
    regionRef          <- newIORef Nothing
    postalCodeRef      <- newIORef Nothing
    countryRef         <- newIORef Nothing
    -- Orders:
    ordersRef          <- newIORef Nothing
    ordersListRef      <- newIORef []
    customerIDRef      <- newIORef Nothing
    employeeIDRef      <- newIORef Nothing
    orderDateRef       <- newIORef Nothing
    requiredDateRef    <- newIORef Nothing
    shipViaRef         <- newIORef Nothing
    freightRef         <- newIORef Nothing
    shipInfoRef        <- newIORef Nothing
    shipNameRef        <- newIORef Nothing
    shipAddressRef     <- newIORef Nothing
    shipCityRef        <- newIORef Nothing
    shipRegionRef      <- newIORef Nothing
    shipPostalCodeRef  <- newIORef Nothing
    shipCountryRef     <- newIORef Nothing
    -- Current tag to field writer:
    textValueWriterRef <- newIORef (\_ -> return ())
    let writeTextToRef' ref f = writeIORef textValueWriterRef (\tagValue -> writeIORef ref (f tagValue))
    let writeTextToRef  ref   = writeTextToRef' ref Just
    --
    flip Xeno.process str (Process
        { openF    = \tagName -> do
            level <- readIORef levelRef
            case (level, tagName) of
              (0, "Root")           -> writeIORef levelRef 1
              (1, "Customers")      -> writeIORef levelRef 2
              (2, "Customer")       -> writeIORef levelRef 3
              (3, "CompanyName")    -> writeTextToRef companyNameRef
              (3, "ContactName")    -> writeTextToRef contactNameRef
              (3, "ContactTitle")   -> writeTextToRef contactTitleRef
              (3, "Phone")          -> writeTextToRef phoneRef
              (3, "Fax")            -> writeTextToRef faxRef
              (3, "FullAddress")    -> writeIORef levelRef 4
              (4, "Address")        -> writeTextToRef addressRef
              (4, "City")           -> writeTextToRef cityRef
              (4, "Region")         -> writeTextToRef regionRef
              (4, "PostalCode")     -> writeTextToRef postalCodeRef
              (4, "Country")        -> writeTextToRef countryRef
              (1, "Orders")         -> writeIORef levelRef 2
              (2, "Order")          -> writeIORef levelRef 3
              (3, "CustomerID")     -> writeTextToRef customerIDRef
              (3, "EmployeeID")     -> writeTextToRef employeeIDRef
              (3, "OrderDate")      -> writeTextToRef' orderDateRef zonedTimeStrM
              (3, "RequiredDate")   -> writeTextToRef' requiredDateRef zonedTimeStrM
              (3, "ShipInfo")       -> writeIORef levelRef 4
              (4, "ShipVia")        -> writeTextToRef' shipViaRef (Just . read . BSC.unpack)
              (4, "Freight")        -> writeTextToRef' freightRef (Just . read . BSC.unpack)
              (4, "ShipName")       -> writeTextToRef shipNameRef
              (4, "ShipAddress")    -> writeTextToRef shipAddressRef
              (4, "ShipCity")       -> writeTextToRef shipCityRef
              (4, "ShipRegion")     -> writeTextToRef shipRegionRef
              (4, "ShipPostalCode") -> writeTextToRef shipPostalCodeRef
              (4, "ShipCountry")    -> writeTextToRef shipCountryRef
              (lvl, tag)            -> error $ concat ["Can't parse open tag; level = ", show lvl, ", tag = '", show tag, "'"]
        , attrF    = \_ _ -> return ()
        , endOpenF = \_ -> return ()
        , textF    = \txt -> readIORef textValueWriterRef >>= \writer -> writer txt
        , closeF   = \tagName -> do
            level <- readIORef levelRef
            case (level, tagName) of
              (1, "Root") -> do
                  Just customers <- readIORef customersRef
                  Just orders    <- readIORef ordersRef
                  writeIORef rootRef $ Just $ Root {..}
                  writeIORef levelRef 0
              (2, "Customers") -> do
                  customersList <- atomicModifyIORef' customersListRef (\list -> ([], list))
                  writeIORef customersRef (Just $ Customers $ reverse customersList)
                  writeIORef levelRef 1
              (2, "Orders") -> do
                  ordersList <- atomicModifyIORef' ordersListRef (\list -> ([], list))
                  writeIORef ordersRef (Just $ Orders $ reverse ordersList)
                  writeIORef levelRef 1
              (3, "Customer") -> do
                  let name = Nothing
                  companyName  <- getAndClear companyNameRef
                  contactName  <- getAndClear contactNameRef
                  contactTitle <- getAndClear contactTitleRef
                  phone        <- getAndClear phoneRef
                  fax          <- getAndClear' faxRef
                  fullAddress  <- getAndClear fullAddressRef
                  modifyIORef' customersListRef (CustomerType {..} : )
                  writeIORef levelRef 2
              (3, "Order") -> do
                  customerID   <- getAndClear customerIDRef
                  employeeID   <- getAndClear employeeIDRef
                  orderDate    <- getAndClear orderDateRef
                  requiredDate <- getAndClear requiredDateRef
                  shipInfo     <- getAndClear shipInfoRef
                  modifyIORef' ordersListRef (OrderType {..} : )
                  writeIORef levelRef 2
              (3, _) -> writeIORef textValueWriterRef (\_ -> return ())
              (4, "FullAddress") -> do
                  let name = Nothing
                  address    <- getAndClear addressRef
                  city       <- getAndClear cityRef
                  region     <- getAndClear regionRef
                  postalCode <- getAndClear postalCodeRef
                  country    <- getAndClear countryRef
                  writeIORef fullAddressRef $ Just $ AddressType {..}
                  writeIORef levelRef 3
              (4, "ShipInfo") -> do
                  let name = Nothing
                  shipVia        <- getAndClear shipViaRef
                  freight        <- getAndClear freightRef
                  shipName       <- getAndClear shipNameRef
                  shipAddress    <- getAndClear shipAddressRef
                  shipCity       <- getAndClear shipCityRef
                  shipRegion     <- getAndClear shipRegionRef
                  shipPostalCode <- getAndClear shipPostalCodeRef
                  shipCountry    <- getAndClear shipCountryRef
                  writeIORef shipInfoRef $ Just $ ShipInfoType {..}
                  writeIORef levelRef 3
              (4, _) -> writeIORef textValueWriterRef (\_ -> return ())
              (lvl, tag) -> error $ concat ["Can't parse close tag; level = ", show lvl, ", tag = '", show tag, "'"]
        , cdataF   = \_ -> return ()
        })
    Just root <- readIORef rootRef
    return root
  where
    -- Get-and-clear: when we use some value from ref for constructing bigger struct,
    -- we no need for contents of this ref, so it can be cleared. For example:
    --
    -- ...
    -- -- 1. Reading <CompanyName>
    -- writeIORef companyNameRef "Veeeeeeery looooooooooooong company name"
    -- ...
    -- ...
    -- -- 2. Construction of Order:
    -- ...
    -- companyName <- readIORef companyNameRef
    -- ...
    -- writeIORef orderRef $ Just $ Order {..}
    -- ...
    -- -- 3. Now we switch to <Customers>, so `companyNameRef` contents
    --       no longer needed.
    --
    -- But do GC can automatically determine that `companyNameRef` is no need?
    --
    -- That is why we use `getAndClear` which can clear ref.
    --
    --
    getAndClear  :: IORef (Maybe a) -> IO a
    getAndClear ref = do
        Just v <- readIORef ref
        writeIORef ref Nothing
        return v
    -- `atomicModifyIORef'` is slower then readIORef / writeIORef
    -- getAndClear ref = atomicModifyIORef' ref (\(Just val) -> (Nothing, val))
    -- Or we can just read ref and no clear it:
    -- getAndClear  ref = fromJust <$> readIORef ref
    getAndClear'  :: IORef (Maybe a) -> IO (Maybe a)
    getAndClear'  ref = do
        v <- readIORef ref
        writeIORef ref Nothing
        return v
    -- getAndClear' ref = atomicModifyIORef' ref (\fax -> (Nothing, fax))
    -- getAndClear' ref = readIORef ref
