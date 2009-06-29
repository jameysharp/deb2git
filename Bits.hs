module Bits (BitGet, BitString, runBitGet, getBits, byteAlign, getBytes, gatherBits, putInt, putBitString) where

import Control.Monad.State
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as L
import Data.Int

data S = S {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int L.ByteString
newtype BitGet a = BitGet { unBitGet :: State S a }
data BitString = BitString L.ByteString !Int

instance Monad BitGet where
    return a = BitGet $ return a
    m >>= f = BitGet $ unBitGet m >>= unBitGet . f
    fail s = BitGet $ fail s

runBitGet :: BitGet a -> L.ByteString -> a
runBitGet (BitGet m) bs = evalState m $ S 0 0 bs

getBits :: Enum e => Int -> BitGet e
getBits = BitGet . liftM toEnum . (get >>=) . align where
    align 0 s = put s >> return 0
    align count (S bytes used str)
        | L.null str = fail "getBits: premature end of input"
        | count < avail = put (S bytes (used + count) str) >> return (bits .&. mask)
        | otherwise = do
            v <- align (count - avail) (S (bytes + 1) 0 (L.tail str))
            return $ (v `shiftL` avail) .|. bits
        where
        avail = bitSize (L.head str) - used
        mask = bit count - 1
        bits = fromEnum (L.head str `shiftR` used)

byteAlign :: BitGet ()
byteAlign = BitGet $ modify advance where
    advance s@(S _ 0 _) = s
    advance (S bytes _ bs) = S (bytes + 1) 0 $ L.tail bs

strictSplitAt :: Int -> L.ByteString -> Maybe (S.ByteString, L.ByteString)
strictSplitAt len = check . L.splitAt (fromIntegral len) where
    check (l, r) = let s = S.concat (L.toChunks l) in
        if S.length s < len then Nothing else Just (s, r)

getBytes :: Int -> BitGet S.ByteString
getBytes count = BitGet $ do
    S bytes used str <- get
    when (used /= 0) $ fail "getBytes: input stream not byte-aligned"
    case strictSplitAt count str of
        Nothing -> fail "getBytes: premature end of input"
        Just (ret, rest) -> do
            put $ S (bytes + fromIntegral count) 0 rest
            return ret

gatherBits :: BitGet a -> BitGet (BitString, a)
gatherBits (BitGet m) = BitGet $ do
    S bytes begin str <- get
    a <- m
    S bytes' end _ <- get
    return (BitString (L.take (bytes' - bytes + if end /= 0 then 1 else 0) str) begin, a)

putInt :: Int -> Put
putInt n | n < 128 = putWord8 (fromIntegral n)
putInt n = do
    putWord8 (fromIntegral n .&. 0x7f .|. 0x80)
    putInt $ n `shiftR` 7

putBitString :: BitString -> Put
putBitString (BitString bs begin) = putWord8 (fromIntegral begin) >> putLazyByteString bs
