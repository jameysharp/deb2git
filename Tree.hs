{-# LANGUAGE DeriveDataTypeable #-}
module Tree where

import Data.Typeable

data Tree meta blob = Tree [(String, meta, Tree meta blob)] | Blob blob
    deriving (Show, Typeable)

newtype AbstractTree blob = AbstractTree (Tree [(String, blob)] blob)
    deriving Show
