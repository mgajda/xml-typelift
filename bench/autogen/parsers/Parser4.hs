{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
module Parser4 (parseMethod4) where


import Data.ByteString (ByteString)
import Data.Char
import Data.Word
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import CustomerOrdersTypes
import Utils


parseMethod4 :: ByteString -> Either String TopLevel
parseMethod4 bs = do
    let start = 0
        rootStart = skipSpaces bs $ skipHeader $ skipSpaces bs start
    if    idx bs rootStart       == fromIntegral (ord '<') -- TODO add a lot of \NUL at the end of string
       && idx bs (rootStart + 1) == fromIntegral (ord 'R')
       && idx bs (rootStart + 2) == fromIntegral (ord 'o')
       && idx bs (rootStart + 3) == fromIntegral (ord 'o')
       && idx bs (rootStart + 4) == fromIntegral (ord 't')
       && idx bs (rootStart + 5) == fromIntegral (ord '>')
    then do
        let customersStart = skipSpaces bs (rootStart + 6)
        (ordersStart', customers) <- parseCustomers customersStart
        let ordersStart = skipSpaces bs ordersStart'
        (rootEndStart', orders)    <- parseOrders ordersStart
        let rootEndStart = skipSpaces bs rootEndStart'
        if    idx bs rootEndStart       == fromIntegral (ord '<')
           && idx bs (rootEndStart + 1) == fromIntegral (ord '/')
           && idx bs (rootEndStart + 2) == fromIntegral (ord 'R')
           && idx bs (rootEndStart + 3) == fromIntegral (ord 'o')
           && idx bs (rootEndStart + 4) == fromIntegral (ord 'o')
           && idx bs (rootEndStart + 5) == fromIntegral (ord 't')
           && idx bs (rootEndStart + 6) == fromIntegral (ord '>')
        then
            return Root {..}
        else
            fail ("Expected '</Root>' but got '" ++ BSC.unpack (ptake bs rootEndStart 7) ++ "'")
    else
        fail ("Expected '<Root>' but got '" ++ BSC.unpack (ptake bs rootStart 6) ++ "'")
  where
    skipHeader ofs
      |    bs `idx` ofs       == openTagChar
        && bs `idx` (ofs + 1) == questionChar
      = skipToCloseTag bs (ofs + 2) + 1
      | otherwise = ofs
    -- *
    failExp expStr ofs = Left ("Expected '" ++ expStr ++ "' but got '" ++ BSC.unpack (ptake bs ofs (length expStr)) ++ "'")
    -- *
    parseCustomers customersStart =
        if    idx bs customersStart       == fromIntegral (ord '<')
           && idx bs (customersStart + 1) == fromIntegral (ord 'C')
           && idx bs (customersStart + 2) == fromIntegral (ord 'u')
           && idx bs (customersStart + 3) == fromIntegral (ord 's')
           && idx bs (customersStart + 4) == fromIntegral (ord 't')
           && idx bs (customersStart + 5) == fromIntegral (ord 'o')
           && idx bs (customersStart + 6) == fromIntegral (ord 'm')
           && idx bs (customersStart + 7) == fromIntegral (ord 'e')
           && idx bs (customersStart + 8) == fromIntegral (ord 'r')
           && idx bs (customersStart + 9) == fromIntegral (ord 's')
           && idx bs (customersStart + 10) == fromIntegral (ord '>')
        then do
            let (customersEnd', customers) = whileRight (customersStart + 11) parseCustomer
                customersStartEnd = skipSpaces bs customersEnd'
            if    idx bs customersStartEnd        == fromIntegral (ord '<')
               && idx bs (customersStartEnd + 1)  == fromIntegral (ord '/')
               && idx bs (customersStartEnd + 2)  == fromIntegral (ord 'C')
               && idx bs (customersStartEnd + 3)  == fromIntegral (ord 'u')
               && idx bs (customersStartEnd + 4)  == fromIntegral (ord 's')
               && idx bs (customersStartEnd + 5)  == fromIntegral (ord 't')
               && idx bs (customersStartEnd + 6)  == fromIntegral (ord 'o')
               && idx bs (customersStartEnd + 7)  == fromIntegral (ord 'm')
               && idx bs (customersStartEnd + 8)  == fromIntegral (ord 'e')
               && idx bs (customersStartEnd + 9)  == fromIntegral (ord 'r')
               && idx bs (customersStartEnd + 10) == fromIntegral (ord 's')
               && idx bs (customersStartEnd + 11) == fromIntegral (ord '>')
            then
                return (customersStartEnd + 12, Customers customers)
            else
                failExp "</Customers>" customersStartEnd
        else
            failExp "<Customers>" customersStart
    -- *
    parseCustomer customerStart' = do
        let customerStart = skipSpaces bs customerStart'
        if    idx bs customerStart       == fromIntegral (ord '<')
           && idx bs (customerStart + 1) == fromIntegral (ord 'C')
           && idx bs (customerStart + 2) == fromIntegral (ord 'u')
           && idx bs (customerStart + 3) == fromIntegral (ord 's')
           && idx bs (customerStart + 4) == fromIntegral (ord 't')
           && idx bs (customerStart + 5) == fromIntegral (ord 'o')
           && idx bs (customerStart + 6) == fromIntegral (ord 'm')
           && idx bs (customerStart + 7) == fromIntegral (ord 'e')
           && idx bs (customerStart + 8) == fromIntegral (ord 'r')
        then do
            let companyNameStart = skipToCloseTag bs (customerStart + 9) + 1 -- TODO extract CustomerID
            (contactNameStart, companyName)  <- parseCompanyName companyNameStart
            (contactTitleStart, contactName) <- parseContactName contactNameStart
            (phoneStart, contactTitle)       <- parseContactTitle contactTitleStart
            (faxStart, phone)                <- parsePhone phoneStart
            (fullAddressStart, fax)          <- parseFax faxStart -- XXX может отсутствовать
            (customerEndStart', fullAddress)  <- parseFullAddress fullAddressStart
            let customerEndStart = skipSpaces bs customerEndStart'
            if    idx bs customerEndStart       == fromIntegral (ord '<')
               && idx bs (customerEndStart + 1) == fromIntegral (ord '/')
               && idx bs (customerEndStart + 2) == fromIntegral (ord 'C')
               && idx bs (customerEndStart + 3) == fromIntegral (ord 'u')
               && idx bs (customerEndStart + 4) == fromIntegral (ord 's')
               && idx bs (customerEndStart + 5) == fromIntegral (ord 't')
               && idx bs (customerEndStart + 6) == fromIntegral (ord 'o')
               && idx bs (customerEndStart + 7) == fromIntegral (ord 'm')
               && idx bs (customerEndStart + 8) == fromIntegral (ord 'e')
               && idx bs (customerEndStart + 9) == fromIntegral (ord 'r')
               && idx bs (customerEndStart + 10) == fromIntegral (ord '>')
            then do
                let name = Nothing -- TODO ???
                return (customerEndStart + 11, CustomerType { .. })
            else
                failExp "</Customer>" customerEndStart
        else
            failExp "<Customer" customerStart
    -- *
    parseCompanyName companyNameStart' = do
        let companyNameStart = skipSpaces bs companyNameStart'
        if    idx bs companyNameStart        == fromIntegral (ord '<')
           && idx bs (companyNameStart + 1)  == fromIntegral (ord 'C')
           && idx bs (companyNameStart + 2)  == fromIntegral (ord 'o')
           && idx bs (companyNameStart + 3)  == fromIntegral (ord 'm')
           && idx bs (companyNameStart + 4)  == fromIntegral (ord 'p')
           && idx bs (companyNameStart + 5)  == fromIntegral (ord 'a')
           && idx bs (companyNameStart + 6)  == fromIntegral (ord 'n')
           && idx bs (companyNameStart + 7)  == fromIntegral (ord 'y')
           && idx bs (companyNameStart + 8)  == fromIntegral (ord 'N')
           && idx bs (companyNameStart + 9)  == fromIntegral (ord 'a')
           && idx bs (companyNameStart + 10) == fromIntegral (ord 'm')
           && idx bs (companyNameStart + 11) == fromIntegral (ord 'e')
           && idx bs (companyNameStart + 12) == fromIntegral (ord '>')
        then do
            let companyNameEnd = skipToOpenTag bs (companyNameStart + 13)
                companyName = substring bs (companyNameStart + 13) companyNameEnd
            if    idx bs companyNameEnd        == fromIntegral (ord '<')
               && idx bs (companyNameEnd + 1)  == fromIntegral (ord '/')
               && idx bs (companyNameEnd + 2)  == fromIntegral (ord 'C')
               && idx bs (companyNameEnd + 3)  == fromIntegral (ord 'o')
               && idx bs (companyNameEnd + 4)  == fromIntegral (ord 'm')
               && idx bs (companyNameEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (companyNameEnd + 6)  == fromIntegral (ord 'a')
               && idx bs (companyNameEnd + 7)  == fromIntegral (ord 'n')
               && idx bs (companyNameEnd + 8)  == fromIntegral (ord 'y')
               && idx bs (companyNameEnd + 9)  == fromIntegral (ord 'N')
               && idx bs (companyNameEnd + 10) == fromIntegral (ord 'a')
               && idx bs (companyNameEnd + 11) == fromIntegral (ord 'm')
               && idx bs (companyNameEnd + 12) == fromIntegral (ord 'e')
               && idx bs (companyNameEnd + 13) == fromIntegral (ord '>')
            then
                return (companyNameEnd + 14, companyName)
            else
                failExp "</CompanyName>" companyNameEnd
        else
            failExp "<CompanyName>" companyNameStart
    -- *
    parseContactName contactNameStart' = do
        let contactNameStart = skipSpaces bs contactNameStart'
        if    idx bs contactNameStart        == fromIntegral (ord '<')
           && idx bs (contactNameStart + 1)  == fromIntegral (ord 'C')
           && idx bs (contactNameStart + 2)  == fromIntegral (ord 'o')
           && idx bs (contactNameStart + 3)  == fromIntegral (ord 'n')
           && idx bs (contactNameStart + 4)  == fromIntegral (ord 't')
           && idx bs (contactNameStart + 5)  == fromIntegral (ord 'a')
           && idx bs (contactNameStart + 6)  == fromIntegral (ord 'c')
           && idx bs (contactNameStart + 7)  == fromIntegral (ord 't')
           && idx bs (contactNameStart + 8)  == fromIntegral (ord 'N')
           && idx bs (contactNameStart + 9)  == fromIntegral (ord 'a')
           && idx bs (contactNameStart + 10) == fromIntegral (ord 'm')
           && idx bs (contactNameStart + 11) == fromIntegral (ord 'e')
           && idx bs (contactNameStart + 12) == fromIntegral (ord '>')
        then do
            let contactNameEnd = skipToOpenTag bs (contactNameStart + 13)
                contactName = substring bs (contactNameStart + 13) contactNameEnd
            if    idx bs contactNameEnd        == fromIntegral (ord '<')
               && idx bs (contactNameEnd + 1)  == fromIntegral (ord '/')
               && idx bs (contactNameEnd + 2)  == fromIntegral (ord 'C')
               && idx bs (contactNameEnd + 3)  == fromIntegral (ord 'o')
               && idx bs (contactNameEnd + 4)  == fromIntegral (ord 'n')
               && idx bs (contactNameEnd + 5)  == fromIntegral (ord 't')
               && idx bs (contactNameEnd + 6)  == fromIntegral (ord 'a')
               && idx bs (contactNameEnd + 7)  == fromIntegral (ord 'c')
               && idx bs (contactNameEnd + 8)  == fromIntegral (ord 't')
               && idx bs (contactNameEnd + 9)  == fromIntegral (ord 'N')
               && idx bs (contactNameEnd + 10) == fromIntegral (ord 'a')
               && idx bs (contactNameEnd + 11) == fromIntegral (ord 'm')
               && idx bs (contactNameEnd + 12) == fromIntegral (ord 'e')
               && idx bs (contactNameEnd + 13) == fromIntegral (ord '>')
            then
                return (contactNameEnd + 14, contactName)
            else
                failExp "</ContactName>" contactNameEnd
        else
            failExp "<ContactName>" contactNameStart
    -- *
    parseContactTitle contactTitleStart' = do
        let contactTitleStart = skipSpaces bs contactTitleStart'
        if    idx bs contactTitleStart        == fromIntegral (ord '<')
           && idx bs (contactTitleStart + 1)  == fromIntegral (ord 'C')
           && idx bs (contactTitleStart + 2)  == fromIntegral (ord 'o')
           && idx bs (contactTitleStart + 3)  == fromIntegral (ord 'n')
           && idx bs (contactTitleStart + 4)  == fromIntegral (ord 't')
           && idx bs (contactTitleStart + 5)  == fromIntegral (ord 'a')
           && idx bs (contactTitleStart + 6)  == fromIntegral (ord 'c')
           && idx bs (contactTitleStart + 7)  == fromIntegral (ord 't')
           && idx bs (contactTitleStart + 8)  == fromIntegral (ord 'T')
           && idx bs (contactTitleStart + 9)  == fromIntegral (ord 'i')
           && idx bs (contactTitleStart + 10) == fromIntegral (ord 't')
           && idx bs (contactTitleStart + 11) == fromIntegral (ord 'l')
           && idx bs (contactTitleStart + 12) == fromIntegral (ord 'e')
           && idx bs (contactTitleStart + 13) == fromIntegral (ord '>')
        then do
            let contactTitleEnd = skipToOpenTag bs (contactTitleStart + 14)
                contactTitle = substring bs (contactTitleStart + 14) contactTitleEnd
            if    idx bs contactTitleEnd        == fromIntegral (ord '<')
               && idx bs (contactTitleEnd + 1)  == fromIntegral (ord '/')
               && idx bs (contactTitleEnd + 2)  == fromIntegral (ord 'C')
               && idx bs (contactTitleEnd + 3)  == fromIntegral (ord 'o')
               && idx bs (contactTitleEnd + 4)  == fromIntegral (ord 'n')
               && idx bs (contactTitleEnd + 5)  == fromIntegral (ord 't')
               && idx bs (contactTitleEnd + 6)  == fromIntegral (ord 'a')
               && idx bs (contactTitleEnd + 7)  == fromIntegral (ord 'c')
               && idx bs (contactTitleEnd + 8)  == fromIntegral (ord 't')
               && idx bs (contactTitleEnd + 9)  == fromIntegral (ord 'T')
               && idx bs (contactTitleEnd + 10) == fromIntegral (ord 'i')
               && idx bs (contactTitleEnd + 11) == fromIntegral (ord 't')
               && idx bs (contactTitleEnd + 12) == fromIntegral (ord 'l')
               && idx bs (contactTitleEnd + 13) == fromIntegral (ord 'e')
               && idx bs (contactTitleEnd + 14) == fromIntegral (ord '>')
            then
                return (contactTitleEnd + 15, contactTitle)
            else
                failExp "</ContactTitle>" contactTitleEnd
        else
            failExp "<ContactTitle>" contactTitleStart
    -- *
    parsePhone phoneStart' = do
        let phoneStart = skipSpaces bs phoneStart'
        if    idx bs phoneStart       == fromIntegral (ord '<')
           && idx bs (phoneStart + 1) == fromIntegral (ord 'P')
           && idx bs (phoneStart + 2) == fromIntegral (ord 'h')
           && idx bs (phoneStart + 3) == fromIntegral (ord 'o')
           && idx bs (phoneStart + 4) == fromIntegral (ord 'n')
           && idx bs (phoneStart + 5) == fromIntegral (ord 'e')
           && idx bs (phoneStart + 6) == fromIntegral (ord '>')
        then do
            let phoneEnd = skipToOpenTag bs (phoneStart + 7)
                phone = substring bs (phoneStart + 7) phoneEnd
            if    idx bs phoneEnd       == fromIntegral (ord '<')
               && idx bs (phoneEnd + 1) == fromIntegral (ord '/')
               && idx bs (phoneEnd + 2) == fromIntegral (ord 'P')
               && idx bs (phoneEnd + 3) == fromIntegral (ord 'h')
               && idx bs (phoneEnd + 4) == fromIntegral (ord 'o')
               && idx bs (phoneEnd + 5) == fromIntegral (ord 'n')
               && idx bs (phoneEnd + 6) == fromIntegral (ord 'e')
               && idx bs (phoneEnd + 7) == fromIntegral (ord '>')
            then
                return (phoneEnd + 8, phone)
            else
                failExp "</Phone>" phoneEnd
        else
            failExp "<Phone>" phoneStart
    parseFax faxStart' = do
        let faxStart = skipSpaces bs faxStart'
        if    idx bs faxStart       == fromIntegral (ord '<')
           && idx bs (faxStart + 1) == fromIntegral (ord 'F')
           && idx bs (faxStart + 2) == fromIntegral (ord 'a')
           && idx bs (faxStart + 3) == fromIntegral (ord 'x')
           && idx bs (faxStart + 4) == fromIntegral (ord '>')
        then do
            let faxEnd = skipToOpenTag bs (faxStart + 5)
                fax = substring bs (faxStart + 5) faxEnd
            if    idx bs faxEnd       == fromIntegral (ord '<')
               && idx bs (faxEnd + 1) == fromIntegral (ord '/')
               && idx bs (faxEnd + 2) == fromIntegral (ord 'F')
               && idx bs (faxEnd + 3) == fromIntegral (ord 'a')
               && idx bs (faxEnd + 4) == fromIntegral (ord 'x')
               && idx bs (faxEnd + 5) == fromIntegral (ord '>')
            then
                return (faxEnd + 6, Just fax)
            else
                failExp "</Fax>" faxEnd
        else
            return (faxStart', Nothing)
    parseFullAddress fullAddressStart' = do
        let fullAddressStart = skipSpaces bs fullAddressStart'
        if    idx bs fullAddressStart       == fromIntegral (ord '<')
           && idx bs (fullAddressStart + 1) == fromIntegral (ord 'F')
           && idx bs (fullAddressStart + 2) == fromIntegral (ord 'u')
           && idx bs (fullAddressStart + 3) == fromIntegral (ord 'l')
           && idx bs (fullAddressStart + 4) == fromIntegral (ord 'l')
           && idx bs (fullAddressStart + 5) == fromIntegral (ord 'A')
           && idx bs (fullAddressStart + 6) == fromIntegral (ord 'd')
           && idx bs (fullAddressStart + 7) == fromIntegral (ord 'd')
           && idx bs (fullAddressStart + 8) == fromIntegral (ord 'r')
           && idx bs (fullAddressStart + 9) == fromIntegral (ord 'e')
           && idx bs (fullAddressStart + 10) == fromIntegral (ord 's')
           && idx bs (fullAddressStart + 11) == fromIntegral (ord 's')
           && idx bs (fullAddressStart + 12) == fromIntegral (ord '>')
        then do
            (cityStart, address)           <- parseAddress (fullAddressStart + 13)
            (regionStart, city)            <- parseCity cityStart
            (postalCodeStart, region)      <- parseRegion regionStart
            (countryStart, postalCode)     <- parsePostalCode postalCodeStart
            (fullAddressEndStart', country) <- parseCountry countryStart
            let fullAddressEndStart = skipSpaces bs fullAddressEndStart'
            if    idx bs fullAddressEndStart       == fromIntegral (ord '<')
               && idx bs (fullAddressEndStart + 1) == fromIntegral (ord '/')
               && idx bs (fullAddressEndStart + 2) == fromIntegral (ord 'F')
               && idx bs (fullAddressEndStart + 3) == fromIntegral (ord 'u')
               && idx bs (fullAddressEndStart + 4) == fromIntegral (ord 'l')
               && idx bs (fullAddressEndStart + 5) == fromIntegral (ord 'l')
               && idx bs (fullAddressEndStart + 6) == fromIntegral (ord 'A')
               && idx bs (fullAddressEndStart + 7) == fromIntegral (ord 'd')
               && idx bs (fullAddressEndStart + 8) == fromIntegral (ord 'd')
               && idx bs (fullAddressEndStart + 9) == fromIntegral (ord 'r')
               && idx bs (fullAddressEndStart + 10) == fromIntegral (ord 'e')
               && idx bs (fullAddressEndStart + 11) == fromIntegral (ord 's')
               && idx bs (fullAddressEndStart + 12) == fromIntegral (ord 's')
               && idx bs (fullAddressEndStart + 13) == fromIntegral (ord '>')
            then do
                let name = Nothing -- TODO ???
                return (fullAddressEndStart + 14, AddressType { .. })
            else
                failExp "</FullAddress>" fullAddressEndStart
        else
            failExp "<FullAddress>" fullAddressStart
    -- *
    parseAddress addressStart' = do
        let addressStart = skipSpaces bs addressStart'
        if    idx bs addressStart       == fromIntegral (ord '<')
           && idx bs (addressStart + 1) == fromIntegral (ord 'A')
           && idx bs (addressStart + 2) == fromIntegral (ord 'd')
           && idx bs (addressStart + 3) == fromIntegral (ord 'd')
           && idx bs (addressStart + 4) == fromIntegral (ord 'r')
           && idx bs (addressStart + 5) == fromIntegral (ord 'e')
           && idx bs (addressStart + 6) == fromIntegral (ord 's')
           && idx bs (addressStart + 7) == fromIntegral (ord 's')
           && idx bs (addressStart + 8) == fromIntegral (ord '>')
        then do
            let addressEnd = skipToOpenTag bs (addressStart + 9)
                address = substring bs (addressStart + 9) addressEnd
            if    idx bs addressEnd       == fromIntegral (ord '<')
               && idx bs (addressEnd + 1) == fromIntegral (ord '/')
               && idx bs (addressEnd + 2) == fromIntegral (ord 'A')
               && idx bs (addressEnd + 3) == fromIntegral (ord 'd')
               && idx bs (addressEnd + 4) == fromIntegral (ord 'd')
               && idx bs (addressEnd + 5) == fromIntegral (ord 'r')
               && idx bs (addressEnd + 6) == fromIntegral (ord 'e')
               && idx bs (addressEnd + 7) == fromIntegral (ord 's')
               && idx bs (addressEnd + 8) == fromIntegral (ord 's')
               && idx bs (addressEnd + 9) == fromIntegral (ord '>')
            then
                return (addressEnd + 10, address)
            else
                failExp "</Address>" addressEnd
        else
            failExp "<Address>" addressStart
    -- *
    parseCity cityStart' = do
        let cityStart = skipSpaces bs cityStart'
        if    idx bs cityStart       == fromIntegral (ord '<')
           && idx bs (cityStart + 1) == fromIntegral (ord 'C')
           && idx bs (cityStart + 2) == fromIntegral (ord 'i')
           && idx bs (cityStart + 3) == fromIntegral (ord 't')
           && idx bs (cityStart + 4) == fromIntegral (ord 'y')
           && idx bs (cityStart + 5) == fromIntegral (ord '>')
        then do
            let cityEnd = skipToOpenTag bs (cityStart + 6)
                city = substring bs (cityStart + 6) cityEnd
            if    idx bs cityEnd       == fromIntegral (ord '<')
               && idx bs (cityEnd + 1) == fromIntegral (ord '/')
               && idx bs (cityEnd + 2) == fromIntegral (ord 'C')
               && idx bs (cityEnd + 3) == fromIntegral (ord 'i')
               && idx bs (cityEnd + 4) == fromIntegral (ord 't')
               && idx bs (cityEnd + 5) == fromIntegral (ord 'y')
               && idx bs (cityEnd + 6) == fromIntegral (ord '>')
            then
                return (cityEnd + 7, city)
            else
                failExp "</City>" cityEnd
        else
            failExp "<City>" cityStart
    -- *
    parseRegion regionStart' = do
        let regionStart = skipSpaces bs regionStart'
        if    idx bs regionStart       == fromIntegral (ord '<')
           && idx bs (regionStart + 1) == fromIntegral (ord 'R')
           && idx bs (regionStart + 2) == fromIntegral (ord 'e')
           && idx bs (regionStart + 3) == fromIntegral (ord 'g')
           && idx bs (regionStart + 4) == fromIntegral (ord 'i')
           && idx bs (regionStart + 5) == fromIntegral (ord 'o')
           && idx bs (regionStart + 6) == fromIntegral (ord 'n')
           && idx bs (regionStart + 7) == fromIntegral (ord '>')
        then do
            let regionEnd = skipToOpenTag bs (regionStart + 8)
                region = substring bs (regionStart + 8) regionEnd
            if    idx bs regionEnd       == fromIntegral (ord '<')
               && idx bs (regionEnd + 1) == fromIntegral (ord '/')
               && idx bs (regionEnd + 2) == fromIntegral (ord 'R')
               && idx bs (regionEnd + 3) == fromIntegral (ord 'e')
               && idx bs (regionEnd + 4) == fromIntegral (ord 'g')
               && idx bs (regionEnd + 5) == fromIntegral (ord 'i')
               && idx bs (regionEnd + 6) == fromIntegral (ord 'o')
               && idx bs (regionEnd + 7) == fromIntegral (ord 'n')
               && idx bs (regionEnd + 8) == fromIntegral (ord '>')
            then
                return (regionEnd + 9, region)
            else
                failExp "</Region>" regionEnd
        else
            failExp "<Region>" regionStart
    -- *
    parsePostalCode postalCodeStart' = do
        let postalCodeStart = skipSpaces bs postalCodeStart'
        if    idx bs postalCodeStart       == fromIntegral (ord '<')
           && idx bs (postalCodeStart + 1) == fromIntegral (ord 'P')
           && idx bs (postalCodeStart + 2) == fromIntegral (ord 'o')
           && idx bs (postalCodeStart + 3) == fromIntegral (ord 's')
           && idx bs (postalCodeStart + 4) == fromIntegral (ord 't')
           && idx bs (postalCodeStart + 5) == fromIntegral (ord 'a')
           && idx bs (postalCodeStart + 6) == fromIntegral (ord 'l')
           && idx bs (postalCodeStart + 7) == fromIntegral (ord 'C')
           && idx bs (postalCodeStart + 8) == fromIntegral (ord 'o')
           && idx bs (postalCodeStart + 9) == fromIntegral (ord 'd')
           && idx bs (postalCodeStart + 10) == fromIntegral (ord 'e')
           && idx bs (postalCodeStart + 11) == fromIntegral (ord '>')
        then do
            let postalCodeEnd = skipToOpenTag bs (postalCodeStart + 12)
                postalCode = substring bs (postalCodeStart + 12) postalCodeEnd
            if    idx bs postalCodeEnd       == fromIntegral (ord '<')
               && idx bs (postalCodeEnd + 1) == fromIntegral (ord '/')
               && idx bs (postalCodeEnd + 2) == fromIntegral (ord 'P')
               && idx bs (postalCodeEnd + 3) == fromIntegral (ord 'o')
               && idx bs (postalCodeEnd + 4) == fromIntegral (ord 's')
               && idx bs (postalCodeEnd + 5) == fromIntegral (ord 't')
               && idx bs (postalCodeEnd + 6) == fromIntegral (ord 'a')
               && idx bs (postalCodeEnd + 7) == fromIntegral (ord 'l')
               && idx bs (postalCodeEnd + 8) == fromIntegral (ord 'C')
               && idx bs (postalCodeEnd + 9) == fromIntegral (ord 'o')
               && idx bs (postalCodeEnd + 10) == fromIntegral (ord 'd')
               && idx bs (postalCodeEnd + 11) == fromIntegral (ord 'e')
               && idx bs (postalCodeEnd + 12) == fromIntegral (ord '>')
            then
                return (postalCodeEnd + 13, postalCode)
            else
                failExp "</PostalCode>" postalCodeEnd
        else
            failExp "<PostalCode>" postalCodeStart
    -- *
    parseCountry countryStart' = do
        let countryStart = skipSpaces bs countryStart'
        if    idx bs countryStart       == fromIntegral (ord '<')
           && idx bs (countryStart + 1) == fromIntegral (ord 'C')
           && idx bs (countryStart + 2) == fromIntegral (ord 'o')
           && idx bs (countryStart + 3) == fromIntegral (ord 'u')
           && idx bs (countryStart + 4) == fromIntegral (ord 'n')
           && idx bs (countryStart + 5) == fromIntegral (ord 't')
           && idx bs (countryStart + 6) == fromIntegral (ord 'r')
           && idx bs (countryStart + 7) == fromIntegral (ord 'y')
           && idx bs (countryStart + 8) == fromIntegral (ord '>')
        then do
            let countryEnd = skipToOpenTag bs (countryStart + 9)
                country = substring bs (countryStart + 9) countryEnd
            if    idx bs countryEnd       == fromIntegral (ord '<')
               && idx bs (countryEnd + 1) == fromIntegral (ord '/')
               && idx bs (countryEnd + 2) == fromIntegral (ord 'C')
               && idx bs (countryEnd + 3) == fromIntegral (ord 'o')
               && idx bs (countryEnd + 4) == fromIntegral (ord 'u')
               && idx bs (countryEnd + 5) == fromIntegral (ord 'n')
               && idx bs (countryEnd + 6) == fromIntegral (ord 't')
               && idx bs (countryEnd + 7) == fromIntegral (ord 'r')
               && idx bs (countryEnd + 8) == fromIntegral (ord 'y')
               && idx bs (countryEnd + 9) == fromIntegral (ord '>')
            then
                return (countryEnd + 10, country)
            else
                failExp "</Country>" countryEnd
        else
            failExp "<Country>" countryStart
    -- *
    parseOrders ordersStart =
        if    idx bs ordersStart       == fromIntegral (ord '<')
           && idx bs (ordersStart + 1) == fromIntegral (ord 'O')
           && idx bs (ordersStart + 2) == fromIntegral (ord 'r')
           && idx bs (ordersStart + 3) == fromIntegral (ord 'd')
           && idx bs (ordersStart + 4) == fromIntegral (ord 'e')
           && idx bs (ordersStart + 5) == fromIntegral (ord 'r')
           && idx bs (ordersStart + 6) == fromIntegral (ord 's')
           && idx bs (ordersStart + 7) == fromIntegral (ord '>')
        then do
            let (ordersEnd', orders) = whileRight (ordersStart + 8) parseOrder
                ordersStartEnd = skipSpaces bs ordersEnd'
            if    idx bs ordersStartEnd       == fromIntegral (ord '<')
               && idx bs (ordersStartEnd + 1) == fromIntegral (ord '/')
               && idx bs (ordersStartEnd + 2) == fromIntegral (ord 'O')
               && idx bs (ordersStartEnd + 3) == fromIntegral (ord 'r')
               && idx bs (ordersStartEnd + 4) == fromIntegral (ord 'd')
               && idx bs (ordersStartEnd + 5) == fromIntegral (ord 'e')
               && idx bs (ordersStartEnd + 6) == fromIntegral (ord 'r')
               && idx bs (ordersStartEnd + 7) == fromIntegral (ord 's')
               && idx bs (ordersStartEnd + 8) == fromIntegral (ord '>')
            then
                return (ordersStartEnd + 9, Orders orders)
            else
                failExp "</Orders>" ordersStartEnd
        else
            failExp "<Orders>" ordersStart
    -- *
    parseOrder orderStart' = do
        let orderStart = skipSpaces bs orderStart'
        if    idx bs orderStart       == fromIntegral (ord '<')
           && idx bs (orderStart + 1) == fromIntegral (ord 'O')
           && idx bs (orderStart + 2) == fromIntegral (ord 'r')
           && idx bs (orderStart + 3) == fromIntegral (ord 'd')
           && idx bs (orderStart + 4) == fromIntegral (ord 'e')
           && idx bs (orderStart + 5) == fromIntegral (ord 'r')
           && idx bs (orderStart + 6) == fromIntegral (ord '>')
        then do
            let customerIDStart = skipSpaces bs (orderStart + 7)
            (employeeIdStart, customerID)  <- parseCustomerID customerIDStart
            (orderDateStart, employeeID)   <- parseEmployeeID employeeIdStart
            (requiredDateStart, orderDate) <- parseOrderDate orderDateStart
            (shipInfoStart, requiredDate)  <- parseRequiredDate requiredDateStart
            (orderEndStart', shipInfo)     <- parseShipInfo shipInfoStart
            let orderEndStart = skipSpaces bs orderEndStart'
            if    idx bs orderEndStart       == fromIntegral (ord '<')
               && idx bs (orderEndStart + 1) == fromIntegral (ord '/')
               && idx bs (orderEndStart + 2) == fromIntegral (ord 'O')
               && idx bs (orderEndStart + 3) == fromIntegral (ord 'r')
               && idx bs (orderEndStart + 4) == fromIntegral (ord 'd')
               && idx bs (orderEndStart + 5) == fromIntegral (ord 'e')
               && idx bs (orderEndStart + 6) == fromIntegral (ord 'r')
               && idx bs (orderEndStart + 7) == fromIntegral (ord '>')
            then
                return (orderEndStart + 8, OrderType { .. })
            else
                failExp "</Order>" orderEndStart
        else
            failExp "<Order>" orderStart
    -- *
    parseCustomerID customerIdStart' = do
        let customerIdStart = skipSpaces bs customerIdStart'
        if    idx bs customerIdStart        == fromIntegral (ord '<')
           && idx bs (customerIdStart + 1)  == fromIntegral (ord 'C')
           && idx bs (customerIdStart + 2)  == fromIntegral (ord 'u')
           && idx bs (customerIdStart + 3)  == fromIntegral (ord 's')
           && idx bs (customerIdStart + 4)  == fromIntegral (ord 't')
           && idx bs (customerIdStart + 5)  == fromIntegral (ord 'o')
           && idx bs (customerIdStart + 6)  == fromIntegral (ord 'm')
           && idx bs (customerIdStart + 7)  == fromIntegral (ord 'e')
           && idx bs (customerIdStart + 8)  == fromIntegral (ord 'r')
           && idx bs (customerIdStart + 9)  == fromIntegral (ord 'I')
           && idx bs (customerIdStart + 10) == fromIntegral (ord 'D')
           && idx bs (customerIdStart + 11) == fromIntegral (ord '>')
        then do
            let customerIdEnd = skipToOpenTag bs (customerIdStart + 12)
                customerId = substring bs (customerIdStart + 12) customerIdEnd
            if    idx bs customerIdEnd        == fromIntegral (ord '<')
               && idx bs (customerIdEnd + 1)  == fromIntegral (ord '/')
               && idx bs (customerIdEnd + 2)  == fromIntegral (ord 'C')
               && idx bs (customerIdEnd + 3)  == fromIntegral (ord 'u')
               && idx bs (customerIdEnd + 4)  == fromIntegral (ord 's')
               && idx bs (customerIdEnd + 5)  == fromIntegral (ord 't')
               && idx bs (customerIdEnd + 6)  == fromIntegral (ord 'o')
               && idx bs (customerIdEnd + 7)  == fromIntegral (ord 'm')
               && idx bs (customerIdEnd + 8)  == fromIntegral (ord 'e')
               && idx bs (customerIdEnd + 9)  == fromIntegral (ord 'r')
               && idx bs (customerIdEnd + 10) == fromIntegral (ord 'I')
               && idx bs (customerIdEnd + 11) == fromIntegral (ord 'D')
               && idx bs (customerIdEnd + 12) == fromIntegral (ord '>')
            then
                return (customerIdEnd + 13, customerId)
            else
                failExp "</CustomerID>" customerIdEnd
        else
            failExp "<CustomerID>" customerIdStart
    -- *
    parseEmployeeID employeeIdStart' = do
        let employeeIdStart = skipSpaces bs employeeIdStart'
        if    idx bs employeeIdStart        == fromIntegral (ord '<')
           && idx bs (employeeIdStart + 1)  == fromIntegral (ord 'E')
           && idx bs (employeeIdStart + 2)  == fromIntegral (ord 'm')
           && idx bs (employeeIdStart + 3)  == fromIntegral (ord 'p')
           && idx bs (employeeIdStart + 4)  == fromIntegral (ord 'l')
           && idx bs (employeeIdStart + 5)  == fromIntegral (ord 'o')
           && idx bs (employeeIdStart + 6)  == fromIntegral (ord 'y')
           && idx bs (employeeIdStart + 7)  == fromIntegral (ord 'e')
           && idx bs (employeeIdStart + 8)  == fromIntegral (ord 'e')
           && idx bs (employeeIdStart + 9)  == fromIntegral (ord 'I')
           && idx bs (employeeIdStart + 10) == fromIntegral (ord 'D')
           && idx bs (employeeIdStart + 11) == fromIntegral (ord '>')
        then do
            let employeeIdEnd = skipToOpenTag bs (employeeIdStart + 12)
                employeeId = substring bs (employeeIdStart + 12) employeeIdEnd
            if    idx bs employeeIdEnd        == fromIntegral (ord '<')
               && idx bs (employeeIdEnd + 1)  == fromIntegral (ord '/')
               && idx bs (employeeIdEnd + 2)  == fromIntegral (ord 'E')
               && idx bs (employeeIdEnd + 3)  == fromIntegral (ord 'm')
               && idx bs (employeeIdEnd + 4)  == fromIntegral (ord 'p')
               && idx bs (employeeIdEnd + 5)  == fromIntegral (ord 'l')
               && idx bs (employeeIdEnd + 6)  == fromIntegral (ord 'o')
               && idx bs (employeeIdEnd + 7)  == fromIntegral (ord 'y')
               && idx bs (employeeIdEnd + 8)  == fromIntegral (ord 'e')
               && idx bs (employeeIdEnd + 9)  == fromIntegral (ord 'e')
               && idx bs (employeeIdEnd + 10) == fromIntegral (ord 'I')
               && idx bs (employeeIdEnd + 11) == fromIntegral (ord 'D')
               && idx bs (employeeIdEnd + 12) == fromIntegral (ord '>')
            then
                return (employeeIdEnd + 13, employeeId)
            else
                failExp "</EmployeeID>" employeeIdEnd
        else
            failExp "<EmployeeID>" employeeIdStart
    -- *
    parseOrderDate orderDateStart' = do
        let orderDateStart = skipSpaces bs orderDateStart'
        if    idx bs orderDateStart        == fromIntegral (ord '<')
           && idx bs (orderDateStart + 1)  == fromIntegral (ord 'O')
           && idx bs (orderDateStart + 2)  == fromIntegral (ord 'r')
           && idx bs (orderDateStart + 3)  == fromIntegral (ord 'd')
           && idx bs (orderDateStart + 4)  == fromIntegral (ord 'e')
           && idx bs (orderDateStart + 5)  == fromIntegral (ord 'r')
           && idx bs (orderDateStart + 6)  == fromIntegral (ord 'D')
           && idx bs (orderDateStart + 7)  == fromIntegral (ord 'a')
           && idx bs (orderDateStart + 8)  == fromIntegral (ord 't')
           && idx bs (orderDateStart + 9)  == fromIntegral (ord 'e')
           && idx bs (orderDateStart + 10) == fromIntegral (ord '>')
        then do
            let orderDateEnd = skipToOpenTag bs (orderDateStart + 11)
                orderDate = substring bs (orderDateStart + 11) orderDateEnd
            if    idx bs orderDateEnd        == fromIntegral (ord '<')
               && idx bs (orderDateEnd + 1)  == fromIntegral (ord '/')
               && idx bs (orderDateEnd + 2)  == fromIntegral (ord 'O')
               && idx bs (orderDateEnd + 3)  == fromIntegral (ord 'r')
               && idx bs (orderDateEnd + 4)  == fromIntegral (ord 'd')
               && idx bs (orderDateEnd + 5)  == fromIntegral (ord 'e')
               && idx bs (orderDateEnd + 6)  == fromIntegral (ord 'r')
               && idx bs (orderDateEnd + 7)  == fromIntegral (ord 'D')
               && idx bs (orderDateEnd + 8)  == fromIntegral (ord 'a')
               && idx bs (orderDateEnd + 9)  == fromIntegral (ord 't')
               && idx bs (orderDateEnd + 10) == fromIntegral (ord 'e')
               && idx bs (orderDateEnd + 11) == fromIntegral (ord '>')
            then
                (orderDateEnd + 12,) <$> zonedTimeStrM orderDate
            else
                failExp "</OrderDate>" orderDateEnd
        else
            failExp "<OrderDate>" orderDateStart
    -- *
    parseRequiredDate requiredDateStart' = do
        let           requiredDateStart = skipSpaces bs requiredDateStart'
        if    idx bs  requiredDateStart       == fromIntegral (ord '<')
           && idx bs (requiredDateStart + 1)  == fromIntegral (ord 'R')
           && idx bs (requiredDateStart + 2)  == fromIntegral (ord 'e')
           && idx bs (requiredDateStart + 3)  == fromIntegral (ord 'q')
           && idx bs (requiredDateStart + 4)  == fromIntegral (ord 'u')
           && idx bs (requiredDateStart + 5)  == fromIntegral (ord 'i')
           && idx bs (requiredDateStart + 6)  == fromIntegral (ord 'r')
           && idx bs (requiredDateStart + 7)  == fromIntegral (ord 'e')
           && idx bs (requiredDateStart + 8)  == fromIntegral (ord 'd')
           && idx bs (requiredDateStart + 9)  == fromIntegral (ord 'D')
           && idx bs (requiredDateStart + 10) == fromIntegral (ord 'a')
           && idx bs (requiredDateStart + 11) == fromIntegral (ord 't')
           && idx bs (requiredDateStart + 12) == fromIntegral (ord 'e')
           && idx bs (requiredDateStart + 13) == fromIntegral (ord '>')
        then do
            let requiredDateEnd = skipToOpenTag bs (requiredDateStart + 14)
                requiredDate = substring bs (requiredDateStart + 14) requiredDateEnd
            if    idx bs requiredDateEnd        == fromIntegral (ord '<')
               && idx bs (requiredDateEnd + 1)  == fromIntegral (ord '/')
               && idx bs (requiredDateEnd + 2)  == fromIntegral (ord 'R')
               && idx bs (requiredDateEnd + 3)  == fromIntegral (ord 'e')
               && idx bs (requiredDateEnd + 4)  == fromIntegral (ord 'q')
               && idx bs (requiredDateEnd + 5)  == fromIntegral (ord 'u')
               && idx bs (requiredDateEnd + 6)  == fromIntegral (ord 'i')
               && idx bs (requiredDateEnd + 7)  == fromIntegral (ord 'r')
               && idx bs (requiredDateEnd + 8)  == fromIntegral (ord 'e')
               && idx bs (requiredDateEnd + 9)  == fromIntegral (ord 'd')
               && idx bs (requiredDateEnd + 10) == fromIntegral (ord 'D')
               && idx bs (requiredDateEnd + 11) == fromIntegral (ord 'a')
               && idx bs (requiredDateEnd + 12) == fromIntegral (ord 't')
               && idx bs (requiredDateEnd + 13) == fromIntegral (ord 'e')
               && idx bs (requiredDateEnd + 14) == fromIntegral (ord '>')
            then
                (requiredDateEnd + 15,) <$> zonedTimeStrM requiredDate
            else
                failExp "</RequiredDate>" requiredDateEnd
        else
            failExp "<RequiredDate>" requiredDateStart
    parseShipInfo shipInfoStart' = do
        let shipInfoStart = skipSpaces bs shipInfoStart'
        if    idx bs shipInfoStart       == fromIntegral (ord '<')
           && idx bs (shipInfoStart + 1) == fromIntegral (ord 'S')
           && idx bs (shipInfoStart + 2) == fromIntegral (ord 'h')
           && idx bs (shipInfoStart + 3) == fromIntegral (ord 'i')
           && idx bs (shipInfoStart + 4) == fromIntegral (ord 'p')
           && idx bs (shipInfoStart + 5) == fromIntegral (ord 'I')
           && idx bs (shipInfoStart + 6) == fromIntegral (ord 'n')
           && idx bs (shipInfoStart + 7) == fromIntegral (ord 'f')
           && idx bs (shipInfoStart + 8) == fromIntegral (ord 'o')
        then do
            let shipViaStart = skipToCloseTag bs (shipInfoStart + 9) + 1 -- TODO extract ShippedDate
            (freightStart, shipVia)            <- parseShipVia shipViaStart
            (shipNameStart, freight)           <- parseFreight freightStart
            (shipAddressStart, shipName)       <- parseShipName shipNameStart
            (shipCityStart, shipAddress)       <- parseShipAddress shipAddressStart
            (shipRegionStart, shipCity)        <- parseShipCity shipCityStart
            (shipPostalCodeStart, shipRegion)  <- parseShipRegion shipRegionStart
            (shipCountryStart, shipPostalCode) <- parseShipPostalCode shipPostalCodeStart
            (shipInfoEndStart', shipCountry)   <- parseShipCountry shipCountryStart
            let shipInfoEndStart = skipSpaces bs shipInfoEndStart'
            if    idx bs shipInfoEndStart        == fromIntegral (ord '<')
               && idx bs (shipInfoEndStart + 1)  == fromIntegral (ord '/')
               && idx bs (shipInfoEndStart + 2)  == fromIntegral (ord 'S')
               && idx bs (shipInfoEndStart + 3)  == fromIntegral (ord 'h')
               && idx bs (shipInfoEndStart + 4)  == fromIntegral (ord 'i')
               && idx bs (shipInfoEndStart + 5)  == fromIntegral (ord 'p')
               && idx bs (shipInfoEndStart + 6)  == fromIntegral (ord 'I')
               && idx bs (shipInfoEndStart + 7)  == fromIntegral (ord 'n')
               && idx bs (shipInfoEndStart + 8)  == fromIntegral (ord 'f')
               && idx bs (shipInfoEndStart + 9)  == fromIntegral (ord 'o')
               && idx bs (shipInfoEndStart + 10) == fromIntegral (ord '>')
            then do
                let name = Nothing -- TODO ???
                return (shipInfoEndStart + 11, ShipInfoType { .. })
            else
                failExp "</ShipInfo>" shipInfoEndStart
        else
            failExp "<ShipInfo" shipInfoStart
    -- *
    parseShipVia shipViaStart' = do
        let shipViaStart = skipSpaces bs shipViaStart'
        if    idx bs shipViaStart       == fromIntegral (ord '<')
           && idx bs (shipViaStart + 1) == fromIntegral (ord 'S')
           && idx bs (shipViaStart + 2) == fromIntegral (ord 'h')
           && idx bs (shipViaStart + 3) == fromIntegral (ord 'i')
           && idx bs (shipViaStart + 4) == fromIntegral (ord 'p')
           && idx bs (shipViaStart + 5) == fromIntegral (ord 'V')
           && idx bs (shipViaStart + 6) == fromIntegral (ord 'i')
           && idx bs (shipViaStart + 7) == fromIntegral (ord 'a')
           && idx bs (shipViaStart + 8) == fromIntegral (ord '>')
        then do
            let shipViaEnd = skipToOpenTag bs (shipViaStart + 9)
                shipVia = substring bs (shipViaStart + 9) shipViaEnd
            if    idx bs shipViaEnd       == fromIntegral (ord '<')
               && idx bs (shipViaEnd + 1) == fromIntegral (ord '/')
               && idx bs (shipViaEnd + 2) == fromIntegral (ord 'S')
               && idx bs (shipViaEnd + 3) == fromIntegral (ord 'h')
               && idx bs (shipViaEnd + 4) == fromIntegral (ord 'i')
               && idx bs (shipViaEnd + 5) == fromIntegral (ord 'p')
               && idx bs (shipViaEnd + 6) == fromIntegral (ord 'V')
               && idx bs (shipViaEnd + 7) == fromIntegral (ord 'i')
               && idx bs (shipViaEnd + 8) == fromIntegral (ord 'a')
               && idx bs (shipViaEnd + 9) == fromIntegral (ord '>')
            then
                return (shipViaEnd + 10, read $ BSC.unpack shipVia)
            else
                failExp "</ShipVia>" shipViaEnd
        else
            failExp "<ShipVia>" shipViaStart
    -- *
    parseFreight freightStart' = do
        let freightStart = skipSpaces bs freightStart'
        if    idx bs freightStart       == fromIntegral (ord '<')
           && idx bs (freightStart + 1) == fromIntegral (ord 'F')
           && idx bs (freightStart + 2) == fromIntegral (ord 'r')
           && idx bs (freightStart + 3) == fromIntegral (ord 'e')
           && idx bs (freightStart + 4) == fromIntegral (ord 'i')
           && idx bs (freightStart + 5) == fromIntegral (ord 'g')
           && idx bs (freightStart + 6) == fromIntegral (ord 'h')
           && idx bs (freightStart + 7) == fromIntegral (ord 't')
           && idx bs (freightStart + 8) == fromIntegral (ord '>')
        then do
            let freightEnd = skipToOpenTag bs (freightStart + 9)
                freight = substring bs (freightStart + 9) freightEnd
            if    idx bs freightEnd       == fromIntegral (ord '<')
               && idx bs (freightEnd + 1) == fromIntegral (ord '/')
               && idx bs (freightEnd + 2) == fromIntegral (ord 'F')
               && idx bs (freightEnd + 3) == fromIntegral (ord 'r')
               && idx bs (freightEnd + 4) == fromIntegral (ord 'e')
               && idx bs (freightEnd + 5) == fromIntegral (ord 'i')
               && idx bs (freightEnd + 6) == fromIntegral (ord 'g')
               && idx bs (freightEnd + 7) == fromIntegral (ord 'h')
               && idx bs (freightEnd + 8) == fromIntegral (ord 't')
               && idx bs (freightEnd + 9) == fromIntegral (ord '>')
            then
                return (freightEnd + 10, read $ BSC.unpack freight)
            else
                failExp "</Freight>" freightEnd
        else
            failExp "<Freight>" freightStart
    -- *
    parseShipName shipNameStart' = do
        let shipNameStart = skipSpaces bs shipNameStart'
        if    idx bs shipNameStart       == fromIntegral (ord '<')
           && idx bs (shipNameStart + 1) == fromIntegral (ord 'S')
           && idx bs (shipNameStart + 2) == fromIntegral (ord 'h')
           && idx bs (shipNameStart + 3) == fromIntegral (ord 'i')
           && idx bs (shipNameStart + 4) == fromIntegral (ord 'p')
           && idx bs (shipNameStart + 5) == fromIntegral (ord 'N')
           && idx bs (shipNameStart + 6) == fromIntegral (ord 'a')
           && idx bs (shipNameStart + 7) == fromIntegral (ord 'm')
           && idx bs (shipNameStart + 8) == fromIntegral (ord 'e')
           && idx bs (shipNameStart + 9) == fromIntegral (ord '>')
        then do
            let shipNameEnd = skipToOpenTag bs (shipNameStart + 10)
                shipName = substring bs (shipNameStart + 10) shipNameEnd
            if    idx bs shipNameEnd       == fromIntegral (ord '<')
               && idx bs (shipNameEnd + 1) == fromIntegral (ord '/')
               && idx bs (shipNameEnd + 2) == fromIntegral (ord 'S')
               && idx bs (shipNameEnd + 3) == fromIntegral (ord 'h')
               && idx bs (shipNameEnd + 4) == fromIntegral (ord 'i')
               && idx bs (shipNameEnd + 5) == fromIntegral (ord 'p')
               && idx bs (shipNameEnd + 6) == fromIntegral (ord 'N')
               && idx bs (shipNameEnd + 7) == fromIntegral (ord 'a')
               && idx bs (shipNameEnd + 8) == fromIntegral (ord 'm')
               && idx bs (shipNameEnd + 9) == fromIntegral (ord 'e')
               && idx bs (shipNameEnd + 10) == fromIntegral (ord '>')
            then
                return (shipNameEnd + 11, shipName)
            else
                failExp "</ShipName>" shipNameEnd
        else
            failExp "<ShipName>" shipNameStart
    -- *
    parseShipAddress shipAddressStart' = do
        let shipAddressStart = skipSpaces bs shipAddressStart'
        if    idx bs shipAddressStart       == fromIntegral (ord '<')
           && idx bs (shipAddressStart + 1) == fromIntegral (ord 'S')
           && idx bs (shipAddressStart + 2) == fromIntegral (ord 'h')
           && idx bs (shipAddressStart + 3) == fromIntegral (ord 'i')
           && idx bs (shipAddressStart + 4) == fromIntegral (ord 'p')
           && idx bs (shipAddressStart + 5) == fromIntegral (ord 'A')
           && idx bs (shipAddressStart + 6) == fromIntegral (ord 'd')
           && idx bs (shipAddressStart + 7) == fromIntegral (ord 'd')
           && idx bs (shipAddressStart + 8) == fromIntegral (ord 'r')
           && idx bs (shipAddressStart + 9) == fromIntegral (ord 'e')
           && idx bs (shipAddressStart + 10) == fromIntegral (ord 's')
           && idx bs (shipAddressStart + 11) == fromIntegral (ord 's')
           && idx bs (shipAddressStart + 12) == fromIntegral (ord '>')
        then do
            let shipAddressEnd = skipToOpenTag bs (shipAddressStart + 13)
                shipAddress = substring bs (shipAddressStart + 13) shipAddressEnd
            if    idx bs shipAddressEnd        == fromIntegral (ord '<')
               && idx bs (shipAddressEnd + 1)  == fromIntegral (ord '/')
               && idx bs (shipAddressEnd + 2)  == fromIntegral (ord 'S')
               && idx bs (shipAddressEnd + 3)  == fromIntegral (ord 'h')
               && idx bs (shipAddressEnd + 4)  == fromIntegral (ord 'i')
               && idx bs (shipAddressEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (shipAddressEnd + 6)  == fromIntegral (ord 'A')
               && idx bs (shipAddressEnd + 7)  == fromIntegral (ord 'd')
               && idx bs (shipAddressEnd + 8)  == fromIntegral (ord 'd')
               && idx bs (shipAddressEnd + 9)  == fromIntegral (ord 'r')
               && idx bs (shipAddressEnd + 10) == fromIntegral (ord 'e')
               && idx bs (shipAddressEnd + 11) == fromIntegral (ord 's')
               && idx bs (shipAddressEnd + 12) == fromIntegral (ord 's')
               && idx bs (shipAddressEnd + 13) == fromIntegral (ord '>')
            then
                return (shipAddressEnd + 14, shipAddress)
            else
                failExp "</ShipAddress>" shipAddressEnd
        else
            failExp "<ShipAddress>" shipAddressStart
    -- *
    parseShipCity shipCityStart' = do
        let shipCityStart = skipSpaces bs shipCityStart'
        if    idx bs shipCityStart       == fromIntegral (ord '<')
           && idx bs (shipCityStart + 1) == fromIntegral (ord 'S')
           && idx bs (shipCityStart + 2) == fromIntegral (ord 'h')
           && idx bs (shipCityStart + 3) == fromIntegral (ord 'i')
           && idx bs (shipCityStart + 4) == fromIntegral (ord 'p')
           && idx bs (shipCityStart + 5) == fromIntegral (ord 'C')
           && idx bs (shipCityStart + 6) == fromIntegral (ord 'i')
           && idx bs (shipCityStart + 7) == fromIntegral (ord 't')
           && idx bs (shipCityStart + 8) == fromIntegral (ord 'y')
           && idx bs (shipCityStart + 9) == fromIntegral (ord '>')
        then do
            let shipCityEnd = skipToOpenTag bs (shipCityStart + 10)
                shipCity = substring bs (shipCityStart + 10) shipCityEnd
            if    idx bs shipCityEnd        == fromIntegral (ord '<')
               && idx bs (shipCityEnd + 1)  == fromIntegral (ord '/')
               && idx bs (shipCityEnd + 2)  == fromIntegral (ord 'S')
               && idx bs (shipCityEnd + 3)  == fromIntegral (ord 'h')
               && idx bs (shipCityEnd + 4)  == fromIntegral (ord 'i')
               && idx bs (shipCityEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (shipCityEnd + 6)  == fromIntegral (ord 'C')
               && idx bs (shipCityEnd + 7)  == fromIntegral (ord 'i')
               && idx bs (shipCityEnd + 8)  == fromIntegral (ord 't')
               && idx bs (shipCityEnd + 9)  == fromIntegral (ord 'y')
               && idx bs (shipCityEnd + 10) == fromIntegral (ord '>')
            then
                return (shipCityEnd + 11, shipCity)
            else
                failExp "</ShipCity>" shipCityEnd
        else
            failExp "<ShipCity>" shipCityStart
    -- *
    parseShipRegion shipRegionStart' = do
        let shipRegionStart = skipSpaces bs shipRegionStart'
        if    idx bs shipRegionStart        == fromIntegral (ord '<')
           && idx bs (shipRegionStart + 1)  == fromIntegral (ord 'S')
           && idx bs (shipRegionStart + 2)  == fromIntegral (ord 'h')
           && idx bs (shipRegionStart + 3)  == fromIntegral (ord 'i')
           && idx bs (shipRegionStart + 4)  == fromIntegral (ord 'p')
           && idx bs (shipRegionStart + 5)  == fromIntegral (ord 'R')
           && idx bs (shipRegionStart + 6)  == fromIntegral (ord 'e')
           && idx bs (shipRegionStart + 7)  == fromIntegral (ord 'g')
           && idx bs (shipRegionStart + 8)  == fromIntegral (ord 'i')
           && idx bs (shipRegionStart + 9)  == fromIntegral (ord 'o')
           && idx bs (shipRegionStart + 10) == fromIntegral (ord 'n')
           && idx bs (shipRegionStart + 11) == fromIntegral (ord '>')
        then do
            let shipRegionEnd = skipToOpenTag bs (shipRegionStart + 12)
                shipRegion = substring bs (shipRegionStart + 12) shipRegionEnd
            if    idx bs shipRegionEnd        == fromIntegral (ord '<')
               && idx bs (shipRegionEnd + 1)  == fromIntegral (ord '/')
               && idx bs (shipRegionEnd + 2)  == fromIntegral (ord 'S')
               && idx bs (shipRegionEnd + 3)  == fromIntegral (ord 'h')
               && idx bs (shipRegionEnd + 4)  == fromIntegral (ord 'i')
               && idx bs (shipRegionEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (shipRegionEnd + 6)  == fromIntegral (ord 'R')
               && idx bs (shipRegionEnd + 7)  == fromIntegral (ord 'e')
               && idx bs (shipRegionEnd + 8)  == fromIntegral (ord 'g')
               && idx bs (shipRegionEnd + 9)  == fromIntegral (ord 'i')
               && idx bs (shipRegionEnd + 10) == fromIntegral (ord 'o')
               && idx bs (shipRegionEnd + 11) == fromIntegral (ord 'n')
               && idx bs (shipRegionEnd + 12) == fromIntegral (ord '>')
            then
                return (shipRegionEnd + 13, shipRegion)
            else
                failExp "</ShipRegion>" shipRegionEnd
        else
            failExp "<ShipRegion>" shipRegionStart
    -- *
    parseShipPostalCode shipPostalCodeStart' = do
        let shipPostalCodeStart = skipSpaces bs shipPostalCodeStart'
        if    idx bs shipPostalCodeStart        == fromIntegral (ord '<')
           && idx bs (shipPostalCodeStart + 1)  == fromIntegral (ord 'S')
           && idx bs (shipPostalCodeStart + 2)  == fromIntegral (ord 'h')
           && idx bs (shipPostalCodeStart + 3)  == fromIntegral (ord 'i')
           && idx bs (shipPostalCodeStart + 4)  == fromIntegral (ord 'p')
           && idx bs (shipPostalCodeStart + 5)  == fromIntegral (ord 'P')
           && idx bs (shipPostalCodeStart + 6)  == fromIntegral (ord 'o')
           && idx bs (shipPostalCodeStart + 7)  == fromIntegral (ord 's')
           && idx bs (shipPostalCodeStart + 8)  == fromIntegral (ord 't')
           && idx bs (shipPostalCodeStart + 9)  == fromIntegral (ord 'a')
           && idx bs (shipPostalCodeStart + 10) == fromIntegral (ord 'l')
           && idx bs (shipPostalCodeStart + 11) == fromIntegral (ord 'C')
           && idx bs (shipPostalCodeStart + 12) == fromIntegral (ord 'o')
           && idx bs (shipPostalCodeStart + 13) == fromIntegral (ord 'd')
           && idx bs (shipPostalCodeStart + 14) == fromIntegral (ord 'e')
           && idx bs (shipPostalCodeStart + 15) == fromIntegral (ord '>')
        then do
            let shipPostalCodeEnd = skipToOpenTag bs (shipPostalCodeStart + 16)
                shipPostalCode = substring bs (shipPostalCodeStart + 16) shipPostalCodeEnd
            if    idx bs shipPostalCodeEnd        == fromIntegral (ord '<')
               && idx bs (shipPostalCodeEnd + 1)  == fromIntegral (ord '/')
               && idx bs (shipPostalCodeEnd + 2)  == fromIntegral (ord 'S')
               && idx bs (shipPostalCodeEnd + 3)  == fromIntegral (ord 'h')
               && idx bs (shipPostalCodeEnd + 4)  == fromIntegral (ord 'i')
               && idx bs (shipPostalCodeEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (shipPostalCodeEnd + 6)  == fromIntegral (ord 'P')
               && idx bs (shipPostalCodeEnd + 7)  == fromIntegral (ord 'o')
               && idx bs (shipPostalCodeEnd + 8)  == fromIntegral (ord 's')
               && idx bs (shipPostalCodeEnd + 9)  == fromIntegral (ord 't')
               && idx bs (shipPostalCodeEnd + 10) == fromIntegral (ord 'a')
               && idx bs (shipPostalCodeEnd + 11) == fromIntegral (ord 'l')
               && idx bs (shipPostalCodeEnd + 12)  == fromIntegral (ord 'C')
               && idx bs (shipPostalCodeEnd + 13)  == fromIntegral (ord 'o')
               && idx bs (shipPostalCodeEnd + 14)  == fromIntegral (ord 'd')
               && idx bs (shipPostalCodeEnd + 15) == fromIntegral (ord 'e')
               && idx bs (shipPostalCodeEnd + 16) == fromIntegral (ord '>')
            then
                return (shipPostalCodeEnd + 17, shipPostalCode)
            else
                failExp "</ShipPostalCode>" shipPostalCodeEnd
        else
            failExp "<ShipPostalCode>" shipPostalCodeStart
    -- *
    parseShipCountry shipCountryStart' = do
        let shipCountryStart = skipSpaces bs shipCountryStart'
        if    idx bs shipCountryStart        == fromIntegral (ord '<')
           && idx bs (shipCountryStart + 1)  == fromIntegral (ord 'S')
           && idx bs (shipCountryStart + 2)  == fromIntegral (ord 'h')
           && idx bs (shipCountryStart + 3)  == fromIntegral (ord 'i')
           && idx bs (shipCountryStart + 4)  == fromIntegral (ord 'p')
           && idx bs (shipCountryStart + 5)  == fromIntegral (ord 'C')
           && idx bs (shipCountryStart + 6)  == fromIntegral (ord 'o')
           && idx bs (shipCountryStart + 7)  == fromIntegral (ord 'u')
           && idx bs (shipCountryStart + 8)  == fromIntegral (ord 'n')
           && idx bs (shipCountryStart + 9)  == fromIntegral (ord 't')
           && idx bs (shipCountryStart + 10) == fromIntegral (ord 'r')
           && idx bs (shipCountryStart + 11) == fromIntegral (ord 'y')
           && idx bs (shipCountryStart + 12) == fromIntegral (ord '>')
        then do
            let shipCountryEnd = skipToOpenTag bs (shipCountryStart + 13)
                shipCountry = substring bs (shipCountryStart + 13) shipCountryEnd
            if    idx bs shipCountryEnd        == fromIntegral (ord '<')
               && idx bs (shipCountryEnd + 1)  == fromIntegral (ord '/')
               && idx bs (shipCountryEnd + 2)  == fromIntegral (ord 'S')
               && idx bs (shipCountryEnd + 3)  == fromIntegral (ord 'h')
               && idx bs (shipCountryEnd + 4)  == fromIntegral (ord 'i')
               && idx bs (shipCountryEnd + 5)  == fromIntegral (ord 'p')
               && idx bs (shipCountryEnd + 6)  == fromIntegral (ord 'C')
               && idx bs (shipCountryEnd + 7)  == fromIntegral (ord 'o')
               && idx bs (shipCountryEnd + 8)  == fromIntegral (ord 'u')
               && idx bs (shipCountryEnd + 9)  == fromIntegral (ord 'n')
               && idx bs (shipCountryEnd + 10) == fromIntegral (ord 't')
               && idx bs (shipCountryEnd + 11) == fromIntegral (ord 'r')
               && idx bs (shipCountryEnd + 12) == fromIntegral (ord 'y')
               && idx bs (shipCountryEnd + 13) == fromIntegral (ord '>')
            then
                return (shipCountryEnd + 14, shipCountry)
            else
                failExp "</ShipCountry>" shipCountryEnd
        else
            failExp "<ShipCountry>" shipCountryStart


whileRight :: Int -> (Int -> Either String (Int, b)) -> (Int, [b])
whileRight start act =
    case act start of
        Left _            -> (start, [])
        Right (start', b) -> let (ofs, bs) = whileRight start' act in (ofs, b : bs)
{-# INLINE whileRight #-}


ptake :: ByteString -> Int -> Int -> ByteString
ptake bs ofs len = BS.take len $ BS.drop ofs bs -- TODO replace with UNSAFE?
{-# INLINE ptake #-}


idx :: ByteString -> Int -> Word8
idx = BSU.unsafeIndex
{-# INLINE idx #-}

isSpaceChar :: Word8 -> Bool
isSpaceChar c = c == 32 || c == 10 || c == 9 || c == 13
{-# INLINE isSpaceChar #-}

-- | A fast space skipping function.
skipSpaces :: ByteString -> Int -> Int
skipSpaces str i
  | isSpaceChar (idx str i) = skipSpaces str (i + 1)
  | otherwise               = i
{-# INLINE skipSpaces #-}


skipToCloseTag :: ByteString -> Int -> Int
skipToCloseTag str i
  | str `idx` i == closeTagChar = i
  | otherwise                   = skipToCloseTag str (i + 1)
{-# INLINE skipToCloseTag #-}


skipToOpenTag :: ByteString -> Int -> Int
skipToOpenTag str i
  | str `idx` i == openTagChar = i
  | otherwise                  = skipToOpenTag str (i + 1)
-- skipToOpenTag str i = i + fromJust (BS.elemIndex openTagChar (BSU.unsafeDrop i str))
{-# INLINE skipToOpenTag #-}


substring :: ByteString -> Int -> Int -> ByteString
substring s start end = BSU.unsafeTake (end - start) (BSU.unsafeDrop start s)
{-# INLINE substring #-}

-- | Open tag character.
openTagChar :: Word8
openTagChar = 60 -- '<'

-- | Close tag character.
closeTagChar :: Word8
closeTagChar = 62 -- '>'


-- | Char for '?'.
questionChar :: Word8
questionChar = 63
