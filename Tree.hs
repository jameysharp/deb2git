{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Tree where

import Control.Monad
import Data.Bits
import Data.Convertible.Base
import Data.Function
import Data.List
import Data.Ord
import Data.Typeable
import Data.Word
import Numeric

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving (Show, Typeable)

newtype AbstractTree blob = AbstractTree (Tree [(String, blob)] blob)
    deriving Show

newtype GitMode = GitMode Word
    deriving Typeable

instance Show GitMode where
    showsPrec _ (GitMode mode) = showOct mode

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

unescapeEquals :: String -> (String, Bool)
unescapeEquals = fst . foldr doUnescape (([], False), True) where
    doUnescape '=' ((rest, True), True) = (('=' : rest, False), True)
    doUnescape '=' ((rest, False), True) = ((rest, True), True)
    doUnescape c ((rest, meta), _) = ((c : rest, meta), False)

blobify :: String -> a -> (String, GitMode, Tree GitMode a)
blobify name content = (name, GitMode 0o100644, Blob content)

gitSort :: [(String, meta, Tree meta blob)] -> [(String, meta, Tree meta blob)]
gitSort = sortBy $ comparing $ \ (name, _, _) -> name

instance Convertible blob GitMode => Convertible (AbstractTree blob) (GitTree blob) where
    safeConvert (AbstractTree (Blob blob)) = return $ GitTree $ Blob blob
    safeConvert (AbstractTree (Tree tree)) = liftM (GitTree . Tree . gitSort) $ foldM splititem [] tree where

        splititem tree' (name, meta, item) = do
            GitTree item' <- safeConvert (AbstractTree item)
            let escaped = escapeEquals name
            return $
                (escaped ++ "=", GitMode 0o040000, Tree $ map (uncurry blobify) meta) :
                (escaped, toGitMode meta item', item') :
                tree'

        toGitMode _ (Tree _) = GitMode 0o040000
        toGitMode meta _ = head $ [ mode | ("mode", safeConvert -> Right mode) <- meta ] ++ [ GitMode 0o100644 ]

instance (Typeable blob, Show blob) => Convertible (GitTree blob) (AbstractTree blob) where
    safeConvert (GitTree (Blob blob)) = return $ AbstractTree $ Blob blob
    safeConvert (GitTree (Tree tree)) = liftM (AbstractTree . Tree) $ uncurry (merge `on` map snd) $ partition fst unescaped where

        unescaped = [ (meta, (name, mode, item)) | (unescapeEquals -> (name, meta), mode, item) <- tree ]

        -- FIXME: these lists are not always in the same order after unescaping
        merge ((name, _, Tree meta) : xs) ((name', _, item) : ys) | name == name' = do
            meta' <- mapM extractmeta meta
            AbstractTree item' <- safeConvert (GitTree item)
            rest <- merge xs ys
            return $ (name, meta', item') : rest
        merge [] [] = return []
        merge _ _ = convError "metadata match failed" tree

        extractmeta (name, _, Blob content) = return (name, content)
        extractmeta blob = convError "bad metadata blob" blob
