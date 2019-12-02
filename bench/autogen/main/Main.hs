import Parser5

import qualified Data.ByteString.Char8 as BSC
import Text.Pretty.Simple

main :: IO ()
main = do
    BSC.readFile "test/customersOrders.xml" >>= parseMethod5 >>= pPrint
