module Huffman (makeHuffmanCodec, HuffmanCodec, getSymbol, putSymbol) where

import Bits

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Ord
import Data.Word

data HuffmanCodec sym = HuffmanCodec {
    tree :: Tree sym,
    table :: UArray sym Word32
}

data Tree sym = Branch (Tree sym) (Tree sym) | Leaf sym | Null

makeTree :: [(sym, Word8)] -> Tree sym
makeTree = fst . subtree 0 where
    subtree _ [] = (Null, [])
    subtree i ((c, l):lcs') | i == l = (Leaf c, lcs')
    subtree i lcs =
        let (t0, lcs0) = subtree (i+1) lcs
            (t1, lcs1) = subtree (i+1) lcs0
        in (Branch t0 t1, lcs1)

lookupSymbol :: Tree sym -> BitGet sym
lookupSymbol (Leaf sym) = return sym
lookupSymbol (Branch t0 t1) = do
    b <- getBits 1
    lookupSymbol $ if b then t1 else t0
lookupSymbol Null = fail "undefined Huffman symbol"

makeTable :: Ix sym => (sym, sym) -> [(sym, Word8)] -> UArray sym Word32
makeTable bnds = array bnds . snd . mapAccumL makeTable' (0,0) where
    makeTable' (prevlen, code) (sym, len) =
        let code' = code `shiftL` (fromIntegral $ len - prevlen)
        in ((len, code' + 1), (sym, (fromIntegral len `shiftL` 16) .|. bitreverse (fromIntegral len) code'))

bitreverse :: Bits b => Int -> b -> b
bitreverse len n = foldr (.|.) 0 [ bit $ len - 1 - idx | idx <- [0 .. len - 1], testBit n idx ]

makeHuffmanCodec :: Ix sym => [(sym, Word8)] -> HuffmanCodec sym
makeHuffmanCodec syms =
    HuffmanCodec {
        tree = makeTree syms'',
        table = makeTable (fst $ head syms', fst $ last syms') syms''
    }
    where
    syms' = filter ((/= 0) . snd) syms
    syms'' = sortBy (comparing snd) syms'

getSymbol :: HuffmanCodec sym -> BitGet sym
getSymbol = lookupSymbol . tree

putSymbol :: (Ix sym, Monad m) => HuffmanCodec sym -> sym -> BitPutT m ()
putSymbol codec sym =
    let v = table codec ! sym
        len = fromIntegral $ v `shiftR` 16
    in putBits len v
