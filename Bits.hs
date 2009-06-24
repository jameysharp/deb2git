module Bits (BitGet, runBitGet, getBits, byteAlign, getBytes) where

import Control.Monad.State.Strict
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as L

data BitString = BitString {-# UNPACK #-} !Int L.ByteString
newtype BitGet a = BitGet { unBitGet :: State BitString a }

instance Monad BitGet where
    return a = BitGet $ return a
    m >>= f = BitGet $ unBitGet m >>= unBitGet . f
    fail s = BitGet $ fail s

runBitGet :: BitGet a -> L.ByteString -> a
runBitGet (BitGet m) bs = evalState m $ BitString 0 bs

getBits :: Enum e => Int -> BitGet e
getBits = BitGet . liftM toEnum . (get >>=) . align where
    align 0 bs = put bs >> return 0
    align count (BitString used str)
        | L.null str = fail "getBits: premature end of input"
        | count < avail = put (BitString (used + count) str) >> return (bits .&. mask)
        | otherwise = do
            v <- align (count - avail) (BitString 0 (L.tail str))
            return $ (v `shiftL` avail) .|. bits
        where
        avail = bitSize (L.head str) - used
        mask = bit count - 1
        bits = fromEnum (L.head str `shiftR` used)

byteAlign :: BitGet ()
byteAlign = BitGet $ modify advance where
    advance s@(BitString 0 _) = s
    advance (BitString _ bs) = BitString 0 $ L.tail bs

strictSplitAt :: Int -> L.ByteString -> Maybe (S.ByteString, L.ByteString)
strictSplitAt len = check . L.splitAt (fromIntegral len) where
    check (l, r) = let s = S.concat (L.toChunks l) in
        if S.length s < len then Nothing else Just (s, r)

getBytes :: Int -> BitGet S.ByteString
getBytes count = BitGet $ do
    BitString used str <- get
    when (used /= 0) $ fail "getBytes: input stream not byte-aligned"
    case strictSplitAt count str of
        Nothing -> fail "getBytes: premature end of input"
        Just (ret, rest) -> do
            put $ BitString 0 rest
            return ret
