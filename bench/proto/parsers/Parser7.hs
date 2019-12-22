{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE RankNTypes   #-}
module Parser7 (parseMethod7, parseToArray7) where


import Control.DeepSeq
import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.Char
import Data.Functor.Identity
import Data.Time.Format
import Data.Time.LocalTime(ZonedTime)
import Data.Word
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

import CustomerOrdersTypes


customerSize :: Int
customerSize = 22


orderSize :: Int
orderSize = 26


-- | Internal representation of TopLevel
data TopLevelInternal = TopLevelInternal !ByteString !(UV.Vector Int) deriving (Generic, NFData, Show)


-- Structure of array:
--
--      0         1         2         3         4                             N         N+1       N+2
-- idx: 012345678901234567890123456789012345678901234...                      01234567890123456789012345678901...
--      |[Customer1 (22 ints) ][Customer2 (22 ints) ]...[CustomerN (22 ints) ]|[Order 1 (26 ints)       ][Order 2 (26 ints)       ]
--      || | | | | |                                                          |
--      || | | | | +-- fullAddress (fields)                                   |
--      || | | | +---- fax (ofs, len)                                         |
--      || | | +------ phone (ofs, len)                                       |
--      || | +-------- contactTitle (ofs, len)                                |
--      || +---------- contactName (ofs, len)                                 |
--      |+------------ companyName (ofs, len)                                 |
--      |                                                                     |
--      +-- count of customers                                                +-- count of orders

parseToArray7 :: ByteString -> Either String TopLevelInternal
parseToArray7 bs = Right $ TopLevelInternal bs $ UV.create $ do
    vec <- UMV.unsafeNew ((BS.length bs `div` 7) * 2) -- minimal tag: <a></a> (7 bytes), 2 places per tag
    _ <- parseRoot vec
    return vec
  where
    parseRoot :: forall s . UMV.STVector s Int -> ST s Int
    parseRoot vec = do
        UMV.unsafeWrite vec (0::Int) (0::Int)
        let rootStart = skipSpaces bs $ skipHeader $ skipSpaces bs 0
        if    idx bs rootStart       == fromIntegral (ord '<') -- TODO add a lot of \NUL at the end of string to skip length checking
           && idx bs (rootStart + 1) == fromIntegral (ord 'R')
           && idx bs (rootStart + 2) == fromIntegral (ord 'o')
           && idx bs (rootStart + 3) == fromIntegral (ord 'o')
           && idx bs (rootStart + 4) == fromIntegral (ord 't')
           && idx bs (rootStart + 5) == fromIntegral (ord '>')
        then do
            (arrayOfsToOrders, customersEnd) <- parseCustomers 0 (skipSpaces bs (rootStart + 6))
            (_, rootEndStart')               <- parseOrders arrayOfsToOrders (skipSpaces bs customersEnd)
            let rootEndStart = skipSpaces bs rootEndStart'
            if    idx bs rootEndStart       == fromIntegral (ord '<')
               && idx bs (rootEndStart + 1) == fromIntegral (ord '/')
               && idx bs (rootEndStart + 2) == fromIntegral (ord 'R')
               && idx bs (rootEndStart + 3) == fromIntegral (ord 'o')
               && idx bs (rootEndStart + 4) == fromIntegral (ord 'o')
               && idx bs (rootEndStart + 5) == fromIntegral (ord 't')
               && idx bs (rootEndStart + 6) == fromIntegral (ord '>')
            then
                return $ rootEndStart + 7
            else
                failExp "</Root>" rootEndStart
        else
            failExp "<Root>" rootStart
      where
        -- *
        parseCustomers arrayCustomersStart stringCustomersStart = do
            if    idx bs stringCustomersStart       == fromIntegral (ord '<')
               && idx bs (stringCustomersStart + 1) == fromIntegral (ord 'C')
               && idx bs (stringCustomersStart + 2) == fromIntegral (ord 'u')
               && idx bs (stringCustomersStart + 3) == fromIntegral (ord 's')
               && idx bs (stringCustomersStart + 4) == fromIntegral (ord 't')
               && idx bs (stringCustomersStart + 5) == fromIntegral (ord 'o')
               && idx bs (stringCustomersStart + 6) == fromIntegral (ord 'm')
               && idx bs (stringCustomersStart + 7) == fromIntegral (ord 'e')
               && idx bs (stringCustomersStart + 8) == fromIntegral (ord 'r')
               && idx bs (stringCustomersStart + 9) == fromIntegral (ord 's')
               && idx bs (stringCustomersStart + 10) == fromIntegral (ord '>')
            then do
                (arrayOffsetToCustomersEnd, stringOffsetToCustomersEnd, customersCount)
                    <- whileElements (arrayCustomersStart + 1) (stringCustomersStart + 11) parseCustomer
                UMV.unsafeWrite vec arrayCustomersStart customersCount
                let customersStartEnd = skipSpaces bs stringOffsetToCustomersEnd
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
                    return (arrayOffsetToCustomersEnd, customersStartEnd + 12)
                else
                    failExp "</Customers>" customersStartEnd
            else
                failExp "<Customers>" stringCustomersStart
        -- *
        parseCustomer arrayCustomerStart stringCustomerStart' = do
            let stringCustomerStart = skipSpaces bs stringCustomerStart'
            if    idx bs stringCustomerStart       == fromIntegral (ord '<')
               && idx bs (stringCustomerStart + 1) == fromIntegral (ord 'C')
               && idx bs (stringCustomerStart + 2) == fromIntegral (ord 'u')
               && idx bs (stringCustomerStart + 3) == fromIntegral (ord 's')
               && idx bs (stringCustomerStart + 4) == fromIntegral (ord 't')
               && idx bs (stringCustomerStart + 5) == fromIntegral (ord 'o')
               && idx bs (stringCustomerStart + 6) == fromIntegral (ord 'm')
               && idx bs (stringCustomerStart + 7) == fromIntegral (ord 'e')
               && idx bs (stringCustomerStart + 8) == fromIntegral (ord 'r')
            then do
                let companyNameStart = skipToCloseTag bs (stringCustomerStart + 9) + 1
                stringCustomerEndStart'
                        <-  parseCompanyName  arrayCustomerStart companyNameStart
                        >>= parseContactName  (arrayCustomerStart + 2)
                        >>= parseContactTitle (arrayCustomerStart + 4)
                        >>= parsePhone        (arrayCustomerStart + 6)
                        >>= parseFax          (arrayCustomerStart + 8)
                        >>= parseFullAddress  (arrayCustomerStart + 10)
                let stringCustomerEndStart = skipSpaces bs stringCustomerEndStart'
                if    idx bs stringCustomerEndStart       == fromIntegral (ord '<')
                   && idx bs (stringCustomerEndStart + 1) == fromIntegral (ord '/')
                   && idx bs (stringCustomerEndStart + 2) == fromIntegral (ord 'C')
                   && idx bs (stringCustomerEndStart + 3) == fromIntegral (ord 'u')
                   && idx bs (stringCustomerEndStart + 4) == fromIntegral (ord 's')
                   && idx bs (stringCustomerEndStart + 5) == fromIntegral (ord 't')
                   && idx bs (stringCustomerEndStart + 6) == fromIntegral (ord 'o')
                   && idx bs (stringCustomerEndStart + 7) == fromIntegral (ord 'm')
                   && idx bs (stringCustomerEndStart + 8) == fromIntegral (ord 'e')
                   && idx bs (stringCustomerEndStart + 9) == fromIntegral (ord 'r')
                   && idx bs (stringCustomerEndStart + 10) == fromIntegral (ord '>')
                then do
                    return $ Just (arrayCustomerStart + customerSize,  stringCustomerEndStart + 11)
                else
                    return Nothing
            else
                return Nothing
        -- *
        parseCompanyName arrayCompanyNameStart companyNameStart' = do
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
                let companyNameStrStart = companyNameStart + 13
                    companyNameStrEnd   = skipToOpenTag bs (companyNameStart + 13)
                if    idx bs companyNameStrEnd        == fromIntegral (ord '<')
                   && idx bs (companyNameStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (companyNameStrEnd + 2)  == fromIntegral (ord 'C')
                   && idx bs (companyNameStrEnd + 3)  == fromIntegral (ord 'o')
                   && idx bs (companyNameStrEnd + 4)  == fromIntegral (ord 'm')
                   && idx bs (companyNameStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (companyNameStrEnd + 6)  == fromIntegral (ord 'a')
                   && idx bs (companyNameStrEnd + 7)  == fromIntegral (ord 'n')
                   && idx bs (companyNameStrEnd + 8)  == fromIntegral (ord 'y')
                   && idx bs (companyNameStrEnd + 9)  == fromIntegral (ord 'N')
                   && idx bs (companyNameStrEnd + 10) == fromIntegral (ord 'a')
                   && idx bs (companyNameStrEnd + 11) == fromIntegral (ord 'm')
                   && idx bs (companyNameStrEnd + 12) == fromIntegral (ord 'e')
                   && idx bs (companyNameStrEnd + 13) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayCompanyNameStart       companyNameStrStart
                    UMV.unsafeWrite vec (arrayCompanyNameStart + 1) (companyNameStrEnd - companyNameStrStart)
                    return $ companyNameStrEnd + 14
                else
                    failExp "</CompanyName>" companyNameStrEnd
            else
                failExp "<CompanyName>" companyNameStart
        -- *
        parseContactName arrayContactNameStart contactNameStart' = do
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
                let contactNameStrStart = contactNameStart + 13
                    contactNameStrEnd = skipToOpenTag bs contactNameStrStart
                if    idx bs  contactNameStrEnd       == fromIntegral (ord '<')
                   && idx bs (contactNameStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (contactNameStrEnd + 2)  == fromIntegral (ord 'C')
                   && idx bs (contactNameStrEnd + 3)  == fromIntegral (ord 'o')
                   && idx bs (contactNameStrEnd + 4)  == fromIntegral (ord 'n')
                   && idx bs (contactNameStrEnd + 5)  == fromIntegral (ord 't')
                   && idx bs (contactNameStrEnd + 6)  == fromIntegral (ord 'a')
                   && idx bs (contactNameStrEnd + 7)  == fromIntegral (ord 'c')
                   && idx bs (contactNameStrEnd + 8)  == fromIntegral (ord 't')
                   && idx bs (contactNameStrEnd + 9)  == fromIntegral (ord 'N')
                   && idx bs (contactNameStrEnd + 10) == fromIntegral (ord 'a')
                   && idx bs (contactNameStrEnd + 11) == fromIntegral (ord 'm')
                   && idx bs (contactNameStrEnd + 12) == fromIntegral (ord 'e')
                   && idx bs (contactNameStrEnd + 13) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayContactNameStart       contactNameStrStart
                    UMV.unsafeWrite vec (arrayContactNameStart + 1) (contactNameStrEnd - contactNameStrStart)
                    return $ contactNameStrEnd + 14
                else
                    failExp "</ContactName>" contactNameStrEnd
            else
                failExp "<ContactName>" contactNameStart
        -- *
        parseContactTitle arrayContactTitleStart contactTitleStart' = do
            let contactTitleStart = skipSpaces bs contactTitleStart'
            if    idx bs  contactTitleStart       == fromIntegral (ord '<')
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
                let contactTitleStrStart = contactTitleStart + 14
                    contactTitleStrEnd   = skipToOpenTag bs contactTitleStrStart
                if    idx bs contactTitleStrEnd        == fromIntegral (ord '<')
                   && idx bs (contactTitleStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (contactTitleStrEnd + 2)  == fromIntegral (ord 'C')
                   && idx bs (contactTitleStrEnd + 3)  == fromIntegral (ord 'o')
                   && idx bs (contactTitleStrEnd + 4)  == fromIntegral (ord 'n')
                   && idx bs (contactTitleStrEnd + 5)  == fromIntegral (ord 't')
                   && idx bs (contactTitleStrEnd + 6)  == fromIntegral (ord 'a')
                   && idx bs (contactTitleStrEnd + 7)  == fromIntegral (ord 'c')
                   && idx bs (contactTitleStrEnd + 8)  == fromIntegral (ord 't')
                   && idx bs (contactTitleStrEnd + 9)  == fromIntegral (ord 'T')
                   && idx bs (contactTitleStrEnd + 10) == fromIntegral (ord 'i')
                   && idx bs (contactTitleStrEnd + 11) == fromIntegral (ord 't')
                   && idx bs (contactTitleStrEnd + 12) == fromIntegral (ord 'l')
                   && idx bs (contactTitleStrEnd + 13) == fromIntegral (ord 'e')
                   && idx bs (contactTitleStrEnd + 14) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayContactTitleStart       contactTitleStrStart
                    UMV.unsafeWrite vec (arrayContactTitleStart + 1) (contactTitleStrEnd - contactTitleStrStart)
                    return $ contactTitleStrEnd + 15
                else
                    failExp "</ContactTitle>" contactTitleStrEnd
            else
                failExp "<ContactTitle>" contactTitleStart
        -- *
        parsePhone arrayPhoneStart phoneStart' = do
            let phoneStart = skipSpaces bs phoneStart'
            if    idx bs phoneStart       == fromIntegral (ord '<')
               && idx bs (phoneStart + 1) == fromIntegral (ord 'P')
               && idx bs (phoneStart + 2) == fromIntegral (ord 'h')
               && idx bs (phoneStart + 3) == fromIntegral (ord 'o')
               && idx bs (phoneStart + 4) == fromIntegral (ord 'n')
               && idx bs (phoneStart + 5) == fromIntegral (ord 'e')
               && idx bs (phoneStart + 6) == fromIntegral (ord '>')
            then do
                let phoneStrStart = phoneStart + 7
                    phoneStrEnd = skipToOpenTag bs phoneStrStart
                if    idx bs phoneStrEnd       == fromIntegral (ord '<')
                   && idx bs (phoneStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (phoneStrEnd + 2) == fromIntegral (ord 'P')
                   && idx bs (phoneStrEnd + 3) == fromIntegral (ord 'h')
                   && idx bs (phoneStrEnd + 4) == fromIntegral (ord 'o')
                   && idx bs (phoneStrEnd + 5) == fromIntegral (ord 'n')
                   && idx bs (phoneStrEnd + 6) == fromIntegral (ord 'e')
                   && idx bs (phoneStrEnd + 7) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayPhoneStart       phoneStrStart
                    UMV.unsafeWrite vec (arrayPhoneStart + 1) (phoneStrEnd - phoneStrStart)
                    return $ phoneStrEnd + 8
                else
                    failExp "</Phone>" phoneStrEnd
            else
                failExp "<Phone>" phoneStart
        parseFax arrayFaxStart faxStart' = do
            let faxStart = skipSpaces bs faxStart'
            if    idx bs faxStart       == fromIntegral (ord '<')
               && idx bs (faxStart + 1) == fromIntegral (ord 'F')
               && idx bs (faxStart + 2) == fromIntegral (ord 'a')
               && idx bs (faxStart + 3) == fromIntegral (ord 'x')
               && idx bs (faxStart + 4) == fromIntegral (ord '>')
            then do
                let faxStrStart = faxStart + 5
                    faxStrEnd   = skipToOpenTag bs faxStrStart
                if    idx bs faxStrEnd       == fromIntegral (ord '<')
                   && idx bs (faxStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (faxStrEnd + 2) == fromIntegral (ord 'F')
                   && idx bs (faxStrEnd + 3) == fromIntegral (ord 'a')
                   && idx bs (faxStrEnd + 4) == fromIntegral (ord 'x')
                   && idx bs (faxStrEnd + 5) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayFaxStart       faxStrStart
                    UMV.unsafeWrite vec (arrayFaxStart + 1) (faxStrEnd - faxStrStart)
                    return $ faxStrEnd + 6
                else
                    failExp "</Fax>" faxStrEnd
            else do
                UMV.unsafeWrite vec arrayFaxStart       0
                UMV.unsafeWrite vec (arrayFaxStart + 1) 0
                return faxStart'
        parseFullAddress arrayAddressStart fullAddressStart' = do
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
                let addressStart = fullAddressStart + 13
                fullAddressEndStart'
                        <-  parseAddress     arrayAddressStart addressStart
                        >>= parseCity       (arrayAddressStart + 2)
                        >>= parseRegion     (arrayAddressStart + 4)
                        >>= parsePostalCode (arrayAddressStart + 6)
                        >>= parseCountry    (arrayAddressStart + 8)
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
                then
                    return $ fullAddressEndStart + 14
                else
                    failExp "</FullAddress>" fullAddressEndStart
            else
                failExp "<FullAddress>" fullAddressStart
        -- *
        parseAddress arrayAddressStart addressStart' = do
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
                let addressStrStart = addressStart + 9
                    addressStrEnd = skipToOpenTag bs addressStrStart
                if    idx bs addressStrEnd       == fromIntegral (ord '<')
                   && idx bs (addressStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (addressStrEnd + 2) == fromIntegral (ord 'A')
                   && idx bs (addressStrEnd + 3) == fromIntegral (ord 'd')
                   && idx bs (addressStrEnd + 4) == fromIntegral (ord 'd')
                   && idx bs (addressStrEnd + 5) == fromIntegral (ord 'r')
                   && idx bs (addressStrEnd + 6) == fromIntegral (ord 'e')
                   && idx bs (addressStrEnd + 7) == fromIntegral (ord 's')
                   && idx bs (addressStrEnd + 8) == fromIntegral (ord 's')
                   && idx bs (addressStrEnd + 9) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayAddressStart       addressStrStart
                    UMV.unsafeWrite vec (arrayAddressStart + 1) (addressStrEnd - addressStrStart)
                    return $ addressStrEnd + 10
                else
                    failExp "</Address>" addressStrEnd
            else
                failExp "<Address>" addressStart
        -- *
        parseCity arrayCityStart cityStart' = do
            let cityStart = skipSpaces bs cityStart'
            if    idx bs cityStart       == fromIntegral (ord '<')
               && idx bs (cityStart + 1) == fromIntegral (ord 'C')
               && idx bs (cityStart + 2) == fromIntegral (ord 'i')
               && idx bs (cityStart + 3) == fromIntegral (ord 't')
               && idx bs (cityStart + 4) == fromIntegral (ord 'y')
               && idx bs (cityStart + 5) == fromIntegral (ord '>')
            then do
                let cityStrStart = cityStart + 6
                    cityStrEnd = skipToOpenTag bs cityStrStart
                if    idx bs cityStrEnd       == fromIntegral (ord '<')
                   && idx bs (cityStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (cityStrEnd + 2) == fromIntegral (ord 'C')
                   && idx bs (cityStrEnd + 3) == fromIntegral (ord 'i')
                   && idx bs (cityStrEnd + 4) == fromIntegral (ord 't')
                   && idx bs (cityStrEnd + 5) == fromIntegral (ord 'y')
                   && idx bs (cityStrEnd + 6) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayCityStart       cityStrStart
                    UMV.unsafeWrite vec (arrayCityStart + 1) (cityStrEnd - cityStrStart)
                    return $ cityStrEnd + 7
                else
                    failExp "</City>" cityStrEnd
            else
                failExp "<City>" cityStart
        -- *
        parseRegion arrayRegionStart regionStart' = do
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
                let regionStrStart = regionStart + 8
                    regionStrEnd = skipToOpenTag bs regionStrStart
                if    idx bs regionStrEnd       == fromIntegral (ord '<')
                   && idx bs (regionStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (regionStrEnd + 2) == fromIntegral (ord 'R')
                   && idx bs (regionStrEnd + 3) == fromIntegral (ord 'e')
                   && idx bs (regionStrEnd + 4) == fromIntegral (ord 'g')
                   && idx bs (regionStrEnd + 5) == fromIntegral (ord 'i')
                   && idx bs (regionStrEnd + 6) == fromIntegral (ord 'o')
                   && idx bs (regionStrEnd + 7) == fromIntegral (ord 'n')
                   && idx bs (regionStrEnd + 8) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayRegionStart       regionStrStart
                    UMV.unsafeWrite vec (arrayRegionStart + 1) (regionStrEnd - regionStrStart)
                    return $ regionStrEnd + 9
                else
                    failExp "</Region>" regionStrEnd
            else
                failExp "<Region>" regionStart
        -- *
        parsePostalCode arrayPostalCodeStart postalCodeStart' = do
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
                let postalCodeStrStart = postalCodeStart + 12
                    postalCodeStrEnd = skipToOpenTag bs postalCodeStrStart
                if    idx bs postalCodeStrEnd       == fromIntegral (ord '<')
                   && idx bs (postalCodeStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (postalCodeStrEnd + 2) == fromIntegral (ord 'P')
                   && idx bs (postalCodeStrEnd + 3) == fromIntegral (ord 'o')
                   && idx bs (postalCodeStrEnd + 4) == fromIntegral (ord 's')
                   && idx bs (postalCodeStrEnd + 5) == fromIntegral (ord 't')
                   && idx bs (postalCodeStrEnd + 6) == fromIntegral (ord 'a')
                   && idx bs (postalCodeStrEnd + 7) == fromIntegral (ord 'l')
                   && idx bs (postalCodeStrEnd + 8) == fromIntegral (ord 'C')
                   && idx bs (postalCodeStrEnd + 9) == fromIntegral (ord 'o')
                   && idx bs (postalCodeStrEnd + 10) == fromIntegral (ord 'd')
                   && idx bs (postalCodeStrEnd + 11) == fromIntegral (ord 'e')
                   && idx bs (postalCodeStrEnd + 12) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayPostalCodeStart       postalCodeStrStart
                    UMV.unsafeWrite vec (arrayPostalCodeStart + 1) (postalCodeStrEnd - postalCodeStrStart)
                    return $ postalCodeStrEnd + 13
                else
                    failExp "</PostalCode>" postalCodeStrEnd
            else
                failExp "<PostalCode>" postalCodeStart
        -- *
        parseCountry arrayCountryStart countryStart' = do
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
                let countryStrStart = countryStart + 9
                    countryStrEnd = skipToOpenTag bs countryStrStart
                if    idx bs countryStrEnd       == fromIntegral (ord '<')
                   && idx bs (countryStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (countryStrEnd + 2) == fromIntegral (ord 'C')
                   && idx bs (countryStrEnd + 3) == fromIntegral (ord 'o')
                   && idx bs (countryStrEnd + 4) == fromIntegral (ord 'u')
                   && idx bs (countryStrEnd + 5) == fromIntegral (ord 'n')
                   && idx bs (countryStrEnd + 6) == fromIntegral (ord 't')
                   && idx bs (countryStrEnd + 7) == fromIntegral (ord 'r')
                   && idx bs (countryStrEnd + 8) == fromIntegral (ord 'y')
                   && idx bs (countryStrEnd + 9) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayCountryStart       countryStrStart
                    UMV.unsafeWrite vec (arrayCountryStart + 1) (countryStrEnd - countryStrStart)
                    return $ countryStrEnd + 10
                else
                    failExp "</Country>" countryStrEnd
            else
                failExp "<Country>" countryStart
        -- *
        parseOrders arrayOrdersStart ordersStart =
            if    idx bs ordersStart       == fromIntegral (ord '<')
               && idx bs (ordersStart + 1) == fromIntegral (ord 'O')
               && idx bs (ordersStart + 2) == fromIntegral (ord 'r')
               && idx bs (ordersStart + 3) == fromIntegral (ord 'd')
               && idx bs (ordersStart + 4) == fromIntegral (ord 'e')
               && idx bs (ordersStart + 5) == fromIntegral (ord 'r')
               && idx bs (ordersStart + 6) == fromIntegral (ord 's')
               && idx bs (ordersStart + 7) == fromIntegral (ord '>')
            then do
                (arrayOrdersEnd, ordersEnd', ordersCount) <- whileElements (arrayOrdersStart + 1) (ordersStart + 8) parseOrder
                UMV.unsafeWrite vec arrayOrdersStart ordersCount
                let ordersStartEnd = skipSpaces bs ordersEnd'
                if    idx bs ordersStartEnd       == fromIntegral (ord '<')
                   && idx bs (ordersStartEnd + 1) == fromIntegral (ord '/')
                   && idx bs (ordersStartEnd + 2) == fromIntegral (ord 'O')
                   && idx bs (ordersStartEnd + 3) == fromIntegral (ord 'r')
                   && idx bs (ordersStartEnd + 4) == fromIntegral (ord 'd')
                   && idx bs (ordersStartEnd + 5) == fromIntegral (ord 'e')
                   && idx bs (ordersStartEnd + 6) == fromIntegral (ord 'r')
                   && idx bs (ordersStartEnd + 7) == fromIntegral (ord 's')
                   && idx bs (ordersStartEnd + 8) == fromIntegral (ord '>')
                then do
                    return (arrayOrdersEnd, ordersStartEnd + 9)
                else
                    failExp "</Orders>" ordersStartEnd
            else
                failExp "<Orders>" ordersStart
        -- *
        parseOrder arrayOrderStart orderStart' = do
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
                orderEndStart'
                        <-  parseCustomerID   arrayOrderStart customerIDStart
                        >>= parseEmployeeID   (arrayOrderStart + 2)
                        >>= parseOrderDate    (arrayOrderStart + 4)
                        >>= parseRequiredDate (arrayOrderStart + 6)
                        >>= parseShipInfo     (arrayOrderStart + 8)
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
                    return $ Just (arrayOrderStart + orderSize, orderEndStart + 8)
                else
                    return Nothing
            else
                return Nothing
        -- *
        parseCustomerID arrayCustomerIdStart customerIdStart' = do
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
                let customerIdStrStart = customerIdStart + 12
                    customerIdStrEnd = skipToOpenTag bs customerIdStrStart
                if    idx bs customerIdStrEnd        == fromIntegral (ord '<')
                   && idx bs (customerIdStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (customerIdStrEnd + 2)  == fromIntegral (ord 'C')
                   && idx bs (customerIdStrEnd + 3)  == fromIntegral (ord 'u')
                   && idx bs (customerIdStrEnd + 4)  == fromIntegral (ord 's')
                   && idx bs (customerIdStrEnd + 5)  == fromIntegral (ord 't')
                   && idx bs (customerIdStrEnd + 6)  == fromIntegral (ord 'o')
                   && idx bs (customerIdStrEnd + 7)  == fromIntegral (ord 'm')
                   && idx bs (customerIdStrEnd + 8)  == fromIntegral (ord 'e')
                   && idx bs (customerIdStrEnd + 9)  == fromIntegral (ord 'r')
                   && idx bs (customerIdStrEnd + 10) == fromIntegral (ord 'I')
                   && idx bs (customerIdStrEnd + 11) == fromIntegral (ord 'D')
                   && idx bs (customerIdStrEnd + 12) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayCustomerIdStart       customerIdStrStart
                    UMV.unsafeWrite vec (arrayCustomerIdStart + 1) (customerIdStrEnd - customerIdStrStart)
                    return $ customerIdStrEnd + 13
                else
                    failExp "</CustomerID>" customerIdStrEnd
            else
                failExp "<CustomerID>" customerIdStart
        -- *
        parseEmployeeID arrayEmployeeIdStart employeeIdStart' = do
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
                let employeeIdStrStart = employeeIdStart + 12
                    employeeIdStrEnd = skipToOpenTag bs employeeIdStrStart
                if    idx bs employeeIdStrEnd        == fromIntegral (ord '<')
                   && idx bs (employeeIdStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (employeeIdStrEnd + 2)  == fromIntegral (ord 'E')
                   && idx bs (employeeIdStrEnd + 3)  == fromIntegral (ord 'm')
                   && idx bs (employeeIdStrEnd + 4)  == fromIntegral (ord 'p')
                   && idx bs (employeeIdStrEnd + 5)  == fromIntegral (ord 'l')
                   && idx bs (employeeIdStrEnd + 6)  == fromIntegral (ord 'o')
                   && idx bs (employeeIdStrEnd + 7)  == fromIntegral (ord 'y')
                   && idx bs (employeeIdStrEnd + 8)  == fromIntegral (ord 'e')
                   && idx bs (employeeIdStrEnd + 9)  == fromIntegral (ord 'e')
                   && idx bs (employeeIdStrEnd + 10) == fromIntegral (ord 'I')
                   && idx bs (employeeIdStrEnd + 11) == fromIntegral (ord 'D')
                   && idx bs (employeeIdStrEnd + 12) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayEmployeeIdStart       employeeIdStrStart
                    UMV.unsafeWrite vec (arrayEmployeeIdStart + 1) (employeeIdStrEnd - employeeIdStrStart)
                    return $ employeeIdStrEnd + 13
                else
                    failExp "</EmployeeID>" employeeIdStrEnd
            else
                failExp "<EmployeeID>" employeeIdStart
        -- *
        parseOrderDate arrayOrderDateStart orderDateStart' = do
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
                let orderDateStrStart = orderDateStart + 11
                    orderDateStrEnd = skipToOpenTag bs orderDateStrStart
                if    idx bs orderDateStrEnd        == fromIntegral (ord '<')
                   && idx bs (orderDateStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (orderDateStrEnd + 2)  == fromIntegral (ord 'O')
                   && idx bs (orderDateStrEnd + 3)  == fromIntegral (ord 'r')
                   && idx bs (orderDateStrEnd + 4)  == fromIntegral (ord 'd')
                   && idx bs (orderDateStrEnd + 5)  == fromIntegral (ord 'e')
                   && idx bs (orderDateStrEnd + 6)  == fromIntegral (ord 'r')
                   && idx bs (orderDateStrEnd + 7)  == fromIntegral (ord 'D')
                   && idx bs (orderDateStrEnd + 8)  == fromIntegral (ord 'a')
                   && idx bs (orderDateStrEnd + 9)  == fromIntegral (ord 't')
                   && idx bs (orderDateStrEnd + 10) == fromIntegral (ord 'e')
                   && idx bs (orderDateStrEnd + 11) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayOrderDateStart       orderDateStrStart
                    UMV.unsafeWrite vec (arrayOrderDateStart + 1) (orderDateStrEnd - orderDateStrStart)
                    return $ orderDateStrEnd + 12
                else
                    failExp "</OrderDate>" orderDateStrEnd
            else
                failExp "<OrderDate>" orderDateStart
        -- *
        parseRequiredDate arrayRequiredDateStart requiredDateStart' = do
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
                let requiredDateStrStart = requiredDateStart + 14
                    requiredDateStrEnd = skipToOpenTag bs requiredDateStrStart
                if    idx bs requiredDateStrEnd        == fromIntegral (ord '<')
                   && idx bs (requiredDateStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (requiredDateStrEnd + 2)  == fromIntegral (ord 'R')
                   && idx bs (requiredDateStrEnd + 3)  == fromIntegral (ord 'e')
                   && idx bs (requiredDateStrEnd + 4)  == fromIntegral (ord 'q')
                   && idx bs (requiredDateStrEnd + 5)  == fromIntegral (ord 'u')
                   && idx bs (requiredDateStrEnd + 6)  == fromIntegral (ord 'i')
                   && idx bs (requiredDateStrEnd + 7)  == fromIntegral (ord 'r')
                   && idx bs (requiredDateStrEnd + 8)  == fromIntegral (ord 'e')
                   && idx bs (requiredDateStrEnd + 9)  == fromIntegral (ord 'd')
                   && idx bs (requiredDateStrEnd + 10) == fromIntegral (ord 'D')
                   && idx bs (requiredDateStrEnd + 11) == fromIntegral (ord 'a')
                   && idx bs (requiredDateStrEnd + 12) == fromIntegral (ord 't')
                   && idx bs (requiredDateStrEnd + 13) == fromIntegral (ord 'e')
                   && idx bs (requiredDateStrEnd + 14) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayRequiredDateStart       requiredDateStrStart
                    UMV.unsafeWrite vec (arrayRequiredDateStart + 1) (requiredDateStrEnd - requiredDateStrStart)
                    return $ requiredDateStrEnd + 15
                else
                    failExp "</RequiredDate>" requiredDateStrEnd
            else
                failExp "<RequiredDate>" requiredDateStart
        parseShipInfo arrayShipInfoStart shipInfoStart' = do
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
                let shipViaStart = skipToCloseTag bs (shipInfoStart + 9) + 1
                shipInfoEndStart'
                        <-  parseShipVia         arrayShipInfoStart shipViaStart
                        >>= parseFreight        (arrayShipInfoStart + 2)
                        >>= parseShipName       (arrayShipInfoStart + 4)
                        >>= parseShipAddress    (arrayShipInfoStart + 6)
                        >>= parseShipCity       (arrayShipInfoStart + 8)
                        >>= parseShipRegion     (arrayShipInfoStart + 10)
                        >>= parseShipPostalCode (arrayShipInfoStart + 12)
                        >>= parseShipCountry    (arrayShipInfoStart + 14)
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
                    return $ shipInfoEndStart + 11
                else
                    failExp "</ShipInfo>" shipInfoEndStart
            else
                -- NB: yes, without '>'
                failExp "<ShipInfo" shipInfoStart
        -- *
        parseShipVia arrayShipViaStart shipViaStart' = do
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
                let shipViaStrStart = shipViaStart + 9
                let shipViaStrEnd = skipToOpenTag bs shipViaStrStart
                if    idx bs shipViaStrEnd       == fromIntegral (ord '<')
                   && idx bs (shipViaStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (shipViaStrEnd + 2) == fromIntegral (ord 'S')
                   && idx bs (shipViaStrEnd + 3) == fromIntegral (ord 'h')
                   && idx bs (shipViaStrEnd + 4) == fromIntegral (ord 'i')
                   && idx bs (shipViaStrEnd + 5) == fromIntegral (ord 'p')
                   && idx bs (shipViaStrEnd + 6) == fromIntegral (ord 'V')
                   && idx bs (shipViaStrEnd + 7) == fromIntegral (ord 'i')
                   && idx bs (shipViaStrEnd + 8) == fromIntegral (ord 'a')
                   && idx bs (shipViaStrEnd + 9) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipViaStart       shipViaStrStart
                    UMV.unsafeWrite vec (arrayShipViaStart + 1) (shipViaStrEnd - shipViaStrStart)
                    return $ shipViaStrEnd + 10
                else
                    failExp "</ShipVia>" shipViaStrEnd
            else
                failExp "<ShipVia>" shipViaStart
        -- *
        parseFreight arrayFreightStart freightStart' = do
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
                let freightStrStart = freightStart + 9
                    freightStrEnd = skipToOpenTag bs freightStrStart
                if    idx bs freightStrEnd       == fromIntegral (ord '<')
                   && idx bs (freightStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (freightStrEnd + 2) == fromIntegral (ord 'F')
                   && idx bs (freightStrEnd + 3) == fromIntegral (ord 'r')
                   && idx bs (freightStrEnd + 4) == fromIntegral (ord 'e')
                   && idx bs (freightStrEnd + 5) == fromIntegral (ord 'i')
                   && idx bs (freightStrEnd + 6) == fromIntegral (ord 'g')
                   && idx bs (freightStrEnd + 7) == fromIntegral (ord 'h')
                   && idx bs (freightStrEnd + 8) == fromIntegral (ord 't')
                   && idx bs (freightStrEnd + 9) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayFreightStart       freightStrStart
                    UMV.unsafeWrite vec (arrayFreightStart + 1) (freightStrEnd - freightStrStart)
                    return $ freightStrEnd + 10
                else
                    failExp "</Freight>" freightStrEnd
            else
                failExp "<Freight>" freightStart
        -- *
        parseShipName arrayShipNameStart shipNameStart' = do
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
                let shipNameStrStart = shipNameStart + 10
                    shipNameStrEnd = skipToOpenTag bs shipNameStrStart
                if    idx bs shipNameStrEnd       == fromIntegral (ord '<')
                   && idx bs (shipNameStrEnd + 1) == fromIntegral (ord '/')
                   && idx bs (shipNameStrEnd + 2) == fromIntegral (ord 'S')
                   && idx bs (shipNameStrEnd + 3) == fromIntegral (ord 'h')
                   && idx bs (shipNameStrEnd + 4) == fromIntegral (ord 'i')
                   && idx bs (shipNameStrEnd + 5) == fromIntegral (ord 'p')
                   && idx bs (shipNameStrEnd + 6) == fromIntegral (ord 'N')
                   && idx bs (shipNameStrEnd + 7) == fromIntegral (ord 'a')
                   && idx bs (shipNameStrEnd + 8) == fromIntegral (ord 'm')
                   && idx bs (shipNameStrEnd + 9) == fromIntegral (ord 'e')
                   && idx bs (shipNameStrEnd + 10) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipNameStart       shipNameStrStart
                    UMV.unsafeWrite vec (arrayShipNameStart + 1) (shipNameStrEnd - shipNameStrStart)
                    return $ shipNameStrEnd + 11
                else
                    failExp "</ShipName>" shipNameStrEnd
            else
                failExp "<ShipName>" shipNameStart
        -- *
        parseShipAddress arrayShipAddressStart shipAddressStart' = do
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
                let shipAddressStrStart = shipAddressStart + 13
                    shipAddressStrEnd = skipToOpenTag bs shipAddressStrStart
                if    idx bs shipAddressStrEnd        == fromIntegral (ord '<')
                   && idx bs (shipAddressStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (shipAddressStrEnd + 2)  == fromIntegral (ord 'S')
                   && idx bs (shipAddressStrEnd + 3)  == fromIntegral (ord 'h')
                   && idx bs (shipAddressStrEnd + 4)  == fromIntegral (ord 'i')
                   && idx bs (shipAddressStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (shipAddressStrEnd + 6)  == fromIntegral (ord 'A')
                   && idx bs (shipAddressStrEnd + 7)  == fromIntegral (ord 'd')
                   && idx bs (shipAddressStrEnd + 8)  == fromIntegral (ord 'd')
                   && idx bs (shipAddressStrEnd + 9)  == fromIntegral (ord 'r')
                   && idx bs (shipAddressStrEnd + 10) == fromIntegral (ord 'e')
                   && idx bs (shipAddressStrEnd + 11) == fromIntegral (ord 's')
                   && idx bs (shipAddressStrEnd + 12) == fromIntegral (ord 's')
                   && idx bs (shipAddressStrEnd + 13) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipAddressStart       shipAddressStrStart
                    UMV.unsafeWrite vec (arrayShipAddressStart + 1) (shipAddressStrEnd - shipAddressStrStart)
                    return $ shipAddressStrEnd + 14
                else
                    failExp "</ShipAddress>" shipAddressStrEnd
            else
                failExp "<ShipAddress>" shipAddressStart
        -- *
        parseShipCity arrayShipCityStart shipCityStart' = do
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
                let shipCityStrStart = shipCityStart + 10
                let shipCityStrEnd = skipToOpenTag bs shipCityStrStart
                if    idx bs shipCityStrEnd        == fromIntegral (ord '<')
                   && idx bs (shipCityStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (shipCityStrEnd + 2)  == fromIntegral (ord 'S')
                   && idx bs (shipCityStrEnd + 3)  == fromIntegral (ord 'h')
                   && idx bs (shipCityStrEnd + 4)  == fromIntegral (ord 'i')
                   && idx bs (shipCityStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (shipCityStrEnd + 6)  == fromIntegral (ord 'C')
                   && idx bs (shipCityStrEnd + 7)  == fromIntegral (ord 'i')
                   && idx bs (shipCityStrEnd + 8)  == fromIntegral (ord 't')
                   && idx bs (shipCityStrEnd + 9)  == fromIntegral (ord 'y')
                   && idx bs (shipCityStrEnd + 10) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipCityStart       shipCityStrStart
                    UMV.unsafeWrite vec (arrayShipCityStart + 1) (shipCityStrEnd - shipCityStrStart)
                    return $ shipCityStrEnd + 11
                else
                    failExp "</ShipCity>" shipCityStrEnd
            else
                failExp "<ShipCity>" shipCityStart
        -- *
        parseShipRegion arrayShipRegionStart shipRegionStart' = do
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
                let shipRegionStrStart = shipRegionStart + 12
                    shipRegionStrEnd = skipToOpenTag bs shipRegionStrStart
                if    idx bs shipRegionStrEnd        == fromIntegral (ord '<')
                   && idx bs (shipRegionStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (shipRegionStrEnd + 2)  == fromIntegral (ord 'S')
                   && idx bs (shipRegionStrEnd + 3)  == fromIntegral (ord 'h')
                   && idx bs (shipRegionStrEnd + 4)  == fromIntegral (ord 'i')
                   && idx bs (shipRegionStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (shipRegionStrEnd + 6)  == fromIntegral (ord 'R')
                   && idx bs (shipRegionStrEnd + 7)  == fromIntegral (ord 'e')
                   && idx bs (shipRegionStrEnd + 8)  == fromIntegral (ord 'g')
                   && idx bs (shipRegionStrEnd + 9)  == fromIntegral (ord 'i')
                   && idx bs (shipRegionStrEnd + 10) == fromIntegral (ord 'o')
                   && idx bs (shipRegionStrEnd + 11) == fromIntegral (ord 'n')
                   && idx bs (shipRegionStrEnd + 12) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipRegionStart       shipRegionStrStart
                    UMV.unsafeWrite vec (arrayShipRegionStart + 1) (shipRegionStrEnd - shipRegionStrStart)
                    return $ shipRegionStrEnd + 13
                else
                    failExp "</ShipRegion>" shipRegionStrEnd
            else
                failExp "<ShipRegion>" shipRegionStart
        -- *
        parseShipPostalCode arrayShipPostalCodeStart shipPostalCodeStart' = do
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
                let shipPostalCodeStrStart = shipPostalCodeStart + 16
                    shipPostalCodeStrEnd = skipToOpenTag bs shipPostalCodeStrStart
                if    idx bs shipPostalCodeStrEnd        == fromIntegral (ord '<')
                   && idx bs (shipPostalCodeStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (shipPostalCodeStrEnd + 2)  == fromIntegral (ord 'S')
                   && idx bs (shipPostalCodeStrEnd + 3)  == fromIntegral (ord 'h')
                   && idx bs (shipPostalCodeStrEnd + 4)  == fromIntegral (ord 'i')
                   && idx bs (shipPostalCodeStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (shipPostalCodeStrEnd + 6)  == fromIntegral (ord 'P')
                   && idx bs (shipPostalCodeStrEnd + 7)  == fromIntegral (ord 'o')
                   && idx bs (shipPostalCodeStrEnd + 8)  == fromIntegral (ord 's')
                   && idx bs (shipPostalCodeStrEnd + 9)  == fromIntegral (ord 't')
                   && idx bs (shipPostalCodeStrEnd + 10) == fromIntegral (ord 'a')
                   && idx bs (shipPostalCodeStrEnd + 11) == fromIntegral (ord 'l')
                   && idx bs (shipPostalCodeStrEnd + 12)  == fromIntegral (ord 'C')
                   && idx bs (shipPostalCodeStrEnd + 13)  == fromIntegral (ord 'o')
                   && idx bs (shipPostalCodeStrEnd + 14)  == fromIntegral (ord 'd')
                   && idx bs (shipPostalCodeStrEnd + 15) == fromIntegral (ord 'e')
                   && idx bs (shipPostalCodeStrEnd + 16) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipPostalCodeStart       shipPostalCodeStrStart
                    UMV.unsafeWrite vec (arrayShipPostalCodeStart + 1) (shipPostalCodeStrEnd - shipPostalCodeStrStart)
                    return $ shipPostalCodeStrEnd + 17
                else
                    failExp "</ShipPostalCode>" shipPostalCodeStrEnd
            else
                failExp "<ShipPostalCode>" shipPostalCodeStart
        -- *
        parseShipCountry arrayShipCountryStart shipCountryStart' = do
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
                let shipCountryStrStart = shipCountryStart + 13
                    shipCountryStrEnd = skipToOpenTag bs shipCountryStrStart
                if    idx bs shipCountryStrEnd        == fromIntegral (ord '<')
                   && idx bs (shipCountryStrEnd + 1)  == fromIntegral (ord '/')
                   && idx bs (shipCountryStrEnd + 2)  == fromIntegral (ord 'S')
                   && idx bs (shipCountryStrEnd + 3)  == fromIntegral (ord 'h')
                   && idx bs (shipCountryStrEnd + 4)  == fromIntegral (ord 'i')
                   && idx bs (shipCountryStrEnd + 5)  == fromIntegral (ord 'p')
                   && idx bs (shipCountryStrEnd + 6)  == fromIntegral (ord 'C')
                   && idx bs (shipCountryStrEnd + 7)  == fromIntegral (ord 'o')
                   && idx bs (shipCountryStrEnd + 8)  == fromIntegral (ord 'u')
                   && idx bs (shipCountryStrEnd + 9)  == fromIntegral (ord 'n')
                   && idx bs (shipCountryStrEnd + 10) == fromIntegral (ord 't')
                   && idx bs (shipCountryStrEnd + 11) == fromIntegral (ord 'r')
                   && idx bs (shipCountryStrEnd + 12) == fromIntegral (ord 'y')
                   && idx bs (shipCountryStrEnd + 13) == fromIntegral (ord '>')
                then do
                    UMV.unsafeWrite vec arrayShipCountryStart       shipCountryStrStart
                    UMV.unsafeWrite vec (arrayShipCountryStart + 1) (shipCountryStrEnd - shipCountryStrStart)
                    return $ shipCountryStrEnd + 14
                else
                    failExp "</ShipCountry>" shipCountryStrEnd
            else
                failExp "<ShipCountry>" shipCountryStart
        -- *
        skipHeader ofs
          |    bs `idx` ofs       == openTagChar
            && bs `idx` (ofs + 1) == questionChar
          = skipToCloseTag bs (ofs + 2) + 1
          | otherwise = ofs
        -- *
        failExp expStr ofs = fail ("Expected '" ++ expStr ++ "' but got '" ++ BSC.unpack (ptake bs ofs (length expStr + 100)) ++ "'")


whileElements :: Int -> Int -> (Int -> Int -> ST s (Maybe (Int, Int))) -> ST s (Int, Int, Int)
whileElements arrayStart strStart act = go arrayStart strStart 0
  where
    go a s cnt = act a s >>= \case
        Nothing       -> return (a, s, cnt)
        Just (a', s') -> go a' s' (cnt + 1)
{-# INLINE whileElements #-}


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


-- TODO XXX NOTE ABOUT LEN
substring :: ByteString -> Int -> Int -> ByteString
substring s start len = BSU.unsafeTake len (BSU.unsafeDrop start s)
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


zonedTimeStr :: ByteString -> ZonedTime
zonedTimeStr = runIdentity . parseTimeM True defaultTimeLocale fmt . BSC.unpack
  where
    fmt = iso8601DateFormat (Just "%H:%M:%S")
{-# INLINE zonedTimeStr #-}


extractTopLevel :: TopLevelInternal -> TopLevel
extractTopLevel (TopLevelInternal bs arr) =
    Root extractCustomers extractOrders
  where
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
        , fullAddress = extractFullAddress (ofs + 10)
        }
    extractFullAddress ofs = AddressType
        { name = Nothing
        , address = extractXmlString ofs
        , city = extractXmlString (ofs + 2)
        , region = extractXmlString (ofs + 4)
        , postalCode = extractXmlString (ofs + 6)
        , country = extractXmlString (ofs + 8)
        }
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
    extractXmlString ofs = substring bs bsofs bslen
        where bsofs = arr `UV.unsafeIndex` ofs
              bslen = arr `UV.unsafeIndex` (ofs + 1)
    extractMaybeXmlString ofs
        | bsofs == 0 = Nothing
        | otherwise  = Just $ substring bs bsofs bslen
        where bsofs = arr `UV.unsafeIndex` ofs
              bslen = arr `UV.unsafeIndex` (ofs + 1)
{-# INLINE extractTopLevel #-}


parseMethod7 :: ByteString -> Either String TopLevel
parseMethod7 = fmap extractTopLevel . parseToArray7
{-# INLINE parseMethod7 #-}

