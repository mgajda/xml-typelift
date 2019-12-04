{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where


import Control.Monad.ST
import Data.ByteString (ByteString)
import Data.Scientific
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Environment
import System.IO
import System.IO.Unsafe
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import qualified Data.Vector as V
import qualified Mutable.Shuffle as M

import CustomerOrdersTypes
import ToXML
import ToXmlInstances()


instance Arbitrary ByteString where
    arbitrary = do
        -- For fast ByteString generation
        -- we use pregenerated long `longPregeneratedRandomString`
        -- and randomly choose substring from it.
        len  <- choose (1, 30)
        from <- choose (0, (B.length longPregeneratedRandomString - 1) - len)
        return $ BU.unsafeTake len $ BU.unsafeDrop from longPregeneratedRandomString


longPregeneratedRandomString :: ByteString
longPregeneratedRandomString =
      BC.pack $ unsafePerformIO
    $ generateWithSeed (mySeed + 1)
    $ shuffle
    $ take maxLength
    $ cycle availableChars
  where
    maxLength = 100 * (length availableChars)
    availableChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " ~!@#$%^*()_-+=;,.'!?"
{-# NOINLINE longPregeneratedRandomString #-}


instance Arbitrary AddressType where
    arbitrary = genericArbitrary
    shrink = genericShrink


instance Arbitrary CustomerType where
    arbitrary = genericArbitrary
    shrink = genericShrink


instance Arbitrary TimeOfDay where
    arbitrary = TimeOfDay <$> choose (0, 23) <*> choose (0, 59) <*> (fromIntegral <$> choose (0::Int, 60))


instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> choose (0, 74000)


instance Arbitrary LocalTime where
    arbitrary = LocalTime <$> arbitrary <*> arbitrary


instance Arbitrary TimeZone where
    -- Generating random string is too slow, so we use fixed timezone name
    arbitrary = TimeZone <$> arbitrary <*> return False <*> return "Z"


instance Arbitrary ZonedTime where
    arbitrary = ZonedTime <$> arbitrary <*> arbitrary


instance Arbitrary Scientific where
    arbitrary = scientific <$> arbitrary <*> arbitrary


instance Arbitrary ShipInfoType where
    arbitrary = genericArbitrary
    shrink = genericShrink


instance Arbitrary OrderType where
    arbitrary = genericArbitrary
    shrink = genericShrink


-- | Generate large list of objects from
--   small list of really random objects.
--   This way is significally faster then
--   full random list.
genSemrandomList :: Arbitrary a => Gen [a]
genSemrandomList = do
    let numRandomObjects = 1000
    uniqCusts <- vectorOf numRandomObjects arbitrary
    -- Here we use vector shuffler for fast (~ 30%) shuffling of array.
    MkGen $ \rnd size -> runST $ do
        v  <- V.unsafeThaw $ V.fromListN size (cycle uniqCusts)
        _  <- M.shuffle v rnd
        v' <- V.unsafeFreeze v
        return $ V.toList v'


instance Arbitrary Customers where
    arbitrary = Customers <$> genSemrandomList


instance Arbitrary Orders where
    arbitrary = Orders <$> genSemrandomList


instance Arbitrary Root where
    arbitrary = genericArbitrary
    shrink = genericShrink


-- | Start `Gen` with own random generator.
--   Standard QuickCheck `generate` uses
--   time-dependent random generator,
--   so output results will be different.
generateWithSeed :: Int -> Gen a -> IO a
generateWithSeed seed (MkGen g) =
  do let r = mkQCGen seed
     return (g r 30)


generate' :: Gen a -> IO a
generate' = generateWithSeed mySeed


mySeed :: Int
mySeed = 42


-- | Size for `resize` function to create ~1Gb output file
--
objSizePerMb :: Int
objSizePerMb = 883


main :: IO ()
main = do
    (sizeInMb':outFilepath:_) <- getArgs
    let sizeInMb = read sizeInMb'
        arbSize = sizeInMb * objSizePerMb
    (root :: Root) <- generate' $ resize arbSize $ arbitrary
    let xml = toXML' defIndent root
    withFile outFilepath WriteMode $ flip B.hPutBuilder xml
