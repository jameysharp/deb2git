import Deflate

import Data.Bits
import qualified Data.ByteString.Lazy as L
import System.Environment

main :: IO ()
main = do
    logfile : _ <- getArgs
    d <- L.getContents
    let d' = L.drop 10 d
    let Just nulIndex = L.elemIndex 0 d'
    let d'' = if ((d `L.index` 3) .&. 0x8) == 0 then d' else L.drop (1 + nulIndex) d'
    let (l, inflated) = inflate d''
    L.putStr inflated
    L.writeFile logfile l
