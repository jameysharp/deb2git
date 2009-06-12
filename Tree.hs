{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Tree where

import Control.Monad
import Data.Bits
import Data.Convertible.Base
import Data.Convertible.Utils
import Data.Typeable
import Data.Word
import Numeric

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving Show

newtype AbstractTree blob = AbstractTree (Tree [(String, blob)] blob)
    deriving Show

newtype GitMode = GitMode Word

instance Show GitMode where
    showsPrec _ (GitMode mode) = showOct mode

instance Typeable GitMode where
    typeOf _ = mkTypeName "GitMode"

instance Convertible [Char] GitMode where
    -- FIXME: handle irregular files
    -- FIXME: use 0o120000 for symlinks
    safeConvert s = case readOct s of
        [(mode, "")] -> return $ GitMode $ if (mode :: Word) .&. 0o100 == 0 then 0o100644 else 0o100755
        _ -> convError "invalid mode" s

newtype GitTree blob = GitTree (Tree GitMode blob)
    deriving Show

escapeEquals :: String -> String
escapeEquals = fst . foldr doEscape ([], True) where
    doEscape '=' (rest, True) = ('=' : '=' : rest, True)
    doEscape c (rest, _) = (c : rest, False)

blobify :: String -> a -> (String, GitMode, Tree GitMode a)
blobify name content = (name, GitMode 0o100644, Blob content)

instance Convertible blob GitMode => Convertible (AbstractTree blob) (GitTree blob) where
    safeConvert (AbstractTree (Blob blob)) = return $ GitTree $ Blob blob
    safeConvert (AbstractTree (Tree tree)) = liftM (GitTree . Tree) $ foldM splititem [] tree where

        splititem tree' (name, meta, item) = do
            GitTree item' <- safeConvert (AbstractTree item)
            let escaped = escapeEquals name
            return $
                (escaped ++ "=", GitMode 0o040000, Tree $ map (uncurry blobify) meta) :
                (escaped, toGitMode meta item', item') :
                tree'

        toGitMode _ (Tree _) = GitMode 0o040000
        toGitMode meta _ = head $ [ mode | ("mode", safeConvert -> Right mode) <- meta ] ++ [ GitMode 0o100644 ]
