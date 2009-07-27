import Gzip

import Data.Binary.Get
import qualified Data.ByteString.Lazy as L
import System.Environment

main :: IO ()
main = do
    logfile : _ <- getArgs
    d <- L.getContents
    let (l, inflated) = runGet readGzip d
    L.putStr inflated
    L.writeFile logfile l
