{-# LANGUAGE MultiParamTypeClasses #-}
module Tree where

import Control.Monad
import Data.Bits
import Data.Convertible.Base
import Data.String
import Data.Time.Clock.POSIX
import Data.Word
import Numeric

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving Show

data PosixMeta =
    PosixMode !Word |
    PosixUID !Word |
    PosixGID !Word |
    PosixUName String |
    PosixGName String |
    PosixMTime !POSIXTime
    deriving Show

newtype PosixTree blob = PosixTree (Tree [PosixMeta] blob)
    deriving Show

type GitMode = Word

newtype GitTree blob = GitTree (Tree GitMode blob)
    deriving Show

escapeEquals :: String -> String
escapeEquals = fst . foldr doEscape ([], True) where
    doEscape '=' (rest, True) = ('=' : '=' : rest, True)
    doEscape c (rest, _) = (c : rest, False)

blobify :: IsString a => String -> String -> (String, GitMode, Tree GitMode a)
blobify name content = (name, 0o100644, Blob $ fromString content)

instance IsString a => Convertible (PosixTree a) (GitTree a) where
    safeConvert (PosixTree (Blob blob)) = return $ GitTree $ Blob blob
    safeConvert (PosixTree (Tree tree)) = liftM (GitTree . Tree) $ foldM splititem [] tree where

        splititem tree' (name, meta, item) = do
            GitTree item' <- safeConvert (PosixTree item)
            let escaped = escapeEquals name
            return $ (escaped ++ "=", 0o100755, Tree $ map toGitMeta meta) : (escaped, toGitMode meta item', item') : tree'

        toGitMeta (PosixMode mode) = blobify "mode" $ showOct mode ""
        toGitMeta (PosixUID uid) = blobify "uid" $ show uid
        toGitMeta (PosixGID gid) = blobify "gid" $ show gid
        toGitMeta (PosixUName uname) = blobify "uname" uname
        toGitMeta (PosixGName gname) = blobify "gname" gname
        toGitMeta (PosixMTime mtime) = blobify "mtime" $ show (truncate mtime :: Integer)

        toGitMode _ (Tree _) = 0o040000
        -- FIXME: handle irregular files
        -- FIXME: use 0o120000 for symlinks
        toGitMode meta _ = head $ [ if mode .&. 0o100 == 0 then 0o100644 else 0o100755 | PosixMode mode <- meta ] ++ [ 0o100644 ]
