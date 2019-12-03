import qualified Data.ByteString.Char8 as BSC
import Text.Pretty.Simple


import Parser5


main :: IO ()
main = BSC.readFile "test/customersOrders.xml" >>= parseMethod5 >>= pPrint

