module Huffman (makeHuffmanDecoder) where

import Bits

import Data.List
import Data.Ord
import Data.Word

data Tree sym = Branch (Tree sym) (Tree sym) | Leaf sym | Null

makeTree :: [(sym, Word8)] -> Tree sym
makeTree = fst . subtree 0 . sortBy (comparing snd) . filter ((/= 0) . snd) where
    subtree _ [] = (Null, [])
    subtree i ((c, l):lcs') | i == l = (Leaf c, lcs')
    subtree i lcs =
        let (t0, lcs0) = subtree (i+1) lcs
            (t1, lcs1) = subtree (i+1) lcs0
        in (Branch t0 t1, lcs1)

getSymbol :: Tree sym -> BitGet sym
getSymbol (Leaf sym) = return sym
getSymbol (Branch t0 t1) = do
    b <- getBits 1
    getSymbol $ if b then t1 else t0
getSymbol Null = fail "undefined Huffman symbol"

makeHuffmanDecoder :: [(sym, Word8)] -> BitGet sym
makeHuffmanDecoder = getSymbol . makeTree
