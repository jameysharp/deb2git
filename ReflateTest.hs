import Deflate

import qualified Data.ByteString.Lazy as L
import System.Environment

main :: IO ()
main = do
    logfile : _ <- getArgs
    d <- L.getContents
    l <- L.readFile logfile
    L.putStr $ reflate d l
