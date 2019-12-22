import qualified Data.ByteString.Char8 as BSC
import Text.Pretty.Simple


import Parser7


main :: IO ()
main = BSC.readFile "test/customersOrders.xml" >>= (pPrint . parseMethod7)

