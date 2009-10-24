import Ar
import Gzip
import Tar
import Tree

import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as Set
import System.Directory
import Control.Monad

debTypeFlags debname = do
    deb <- L.readFile debname
    let unarchive (Archive archive) = archive
    return $ Set.fromList
	[ L.head f
	| ("data.tar.gz", _, Just gz) <- unarchive $ readAr deb,
	  (_, ("typeflag", f) : _ , _) <- unarchive $ readTar $ snd $ runGet readGzip gz ]

main = do
    let dir = "/var/cache/apt/archives/"
    debs <- getDirectoryContents dir
    summary <- liftM Set.unions $ forM [ dir ++ name | name@('b':_) <- debs ] $ \ path -> do
	putStr (path ++ ": ")
	s <- debTypeFlags path
	print s
	return s
    print summary
