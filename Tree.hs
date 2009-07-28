{-# LANGUAGE DeriveDataTypeable #-}
module Tree where

import Data.List
import Data.Typeable
import System.FilePath

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving (Show, Typeable)

type AbstractMeta blob = [(String, blob)]

newtype AbstractTree blob = AbstractTree (Tree (AbstractMeta blob) blob)
    deriving Show

newtype Archive blob = Archive [(String, AbstractMeta blob, blob)]
    deriving Show

archiveToTree :: ((String, AbstractMeta blob, blob) -> Tree (AbstractMeta blob) blob) -> Archive blob -> ([String], AbstractTree blob)
archiveToTree mktree (Archive files) = (map (\ (a,_,_) -> a) files, AbstractTree $ Tree $ foldl (flip merge) [] files) where
    merge file@(path, meta, _) = bypath $ splitDirectories $ normalise path where
        bypath [] _ = error "empty path"
        bypath [name] prev = (name, meta, mktree file) : prev
        bypath (dirname : names) prev = byname prev where
            byname [] = error "no such directory"
            byname ((name, meta', ~(Tree tree)) : xs) | name == dirname = (name, meta', Tree $ bypath names tree) : xs
            byname (x : xs) = x : byname xs

treeToArchive :: blob -> ([String], AbstractTree blob) -> Archive blob
treeToArchive emptyblob (files, AbstractTree root) = Archive $ map (reblob . pull) files where
    reblob (fullname, meta, tree) = (fullname, meta, case tree of Blob b -> b; Tree _ -> emptyblob)
    pull fullname = foldl locate (fullname, [], root) $ splitDirectories $ normalise fullname
    findentry (Tree tree) name = find (\ (e, _, _) -> e == name) tree
    findentry _ _ = Nothing
    locate (fullname, _, tree) name = case findentry tree name of
        Just (_, meta, subtree) -> (fullname, meta, subtree)
        _ -> error $ "file \"" ++ fullname ++ "\" not found in tree"
