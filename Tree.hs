{-# LANGUAGE DeriveDataTypeable #-}
module Tree where

import Data.Typeable
import System.FilePath

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving (Show, Typeable)

type AbstractMeta blob = [(String, blob)]

newtype AbstractTree blob = AbstractTree (Tree (AbstractMeta blob) blob)
    deriving Show

newtype Archive blob = Archive [(String, AbstractMeta blob, blob)]
    deriving Show

archiveToTree :: ((String, AbstractMeta blob, blob) -> Tree (AbstractMeta blob) blob) -> Archive blob -> AbstractTree blob
archiveToTree mktree (Archive files) = AbstractTree $ Tree $ foldl (flip merge) [] files where
    merge file@(path, meta, _) = bypath $ splitDirectories $ normalise path where
        bypath [] _ = error "empty path"
        bypath [name] prev = (name, meta, mktree file) : prev
        bypath (dirname : names) prev = byname prev where
            byname [] = error "no such directory"
            byname ((name, meta', ~(Tree tree)) : xs) | name == dirname = (name, meta', Tree $ bypath names tree) : xs
            byname (x : xs) = x : byname xs
