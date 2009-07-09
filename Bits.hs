module Bits (
    BitGet, runBitGet, bitOffset, getBits, skipBits, byteAlign, getInt, getBytes,
    BitString, gatherBits,
    BitPutT, runBitPutT, execBitPutT, bitPutOffset, putBits, putInt, putBitString, putBytes
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Builder
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word

data S = S {-# UNPACK #-} !Int64 {-# UNPACK #-} !Int L.ByteString
newtype BitGet a = BitGet { unBitGet :: State S a }
data BitString = BitString L.ByteString !Int !Int

instance Monad BitGet where
    return a = BitGet $ return a
    m >>= f = BitGet $ unBitGet m >>= unBitGet . f
    fail s = BitGet $ fail s

runBitGet :: BitGet a -> L.ByteString -> a
runBitGet (BitGet m) bs = evalState m $ S 0 0 bs

bitOffset :: BitGet Int
bitOffset = BitGet $ do
    S _ used _ <- get
    return used

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

skipBits :: Int -> BitGet ()
skipBits n = do
    getBits n :: BitGet Int
    return ()

byteAlign :: BitGet ()
byteAlign = BitGet $ modify advance where
    advance s@(S _ 0 _) = s
    advance (S bytes _ bs) = S (bytes + 1) 0 $ L.tail bs

getInt :: BitGet Int
getInt = do
    byte <- getBits 8
    if testBit byte 7
        then do
            rest <- getInt
            return $ (rest `shiftL` 7) .|. (byte .&. 0x7f)
        else return byte

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
    return (BitString (L.take (bytes' - bytes + if end /= 0 then 1 else 0) str) begin end, a)

newtype BitPutT m a = BitPutT { unBitPutT :: StateT (Int, Word8) (WriterT Builder m) a }

instance Monad m => Monad (BitPutT m) where
    return = BitPutT . return
    m >>= f = BitPutT $ unBitPutT m >>= unBitPutT . f
    fail = BitPutT . fail

instance MonadTrans BitPutT where
    lift = BitPutT . lift . lift

runBitPutT :: Monad m => BitPutT m a -> m (a, L.ByteString)
runBitPutT (BitPutT m) = do
    (a, builder) <- runWriterT $ do
        (a, (used, byte)) <- runStateT m (0, 0)
        when (used /= 0) $ tell $ singleton byte
        return a
    return (a, toLazyByteString builder)

execBitPutT :: Monad m => BitPutT m a -> m L.ByteString
execBitPutT = liftM snd . runBitPutT

bitPutOffset :: Monad m => BitPutT m Int
bitPutOffset = BitPutT $ gets fst

putBits :: (Monad m, Enum e) => Int -> e -> BitPutT m ()
putBits count e = BitPutT $ do
    (used, byte) <- get
    let masked = (fromEnum e) .&. (bit count - 1)
    putBits' ((fromIntegral masked `shiftL` used) .|. byte) (count - (8 - used)) (masked `shiftR` (8 - used))
    where
    putBits' byte c v
        | c >= 0 = do
            tell $ singleton byte
            putBits' (fromIntegral v) (c - 8) (v `shiftR` 8)
        | otherwise = put (c + 8, byte)

putInt :: Monad m => Int -> BitPutT m ()
putInt n | n < 128 = putBits 8 n
putInt n = do
    putBits 8 $ n .&. 0x7f .|. 0x80
    putInt $ n `shiftR` 7

putBitString :: Monad m => BitString -> BitPutT m ()
putBitString (BitString bs 0 0) = putBytes bs
putBitString (BitString bs 0 end) = do
    putBytes $ L.init bs
    putBits end $ L.last bs
putBitString (BitString bs begin end) = do
    let Just (h, t) = L.uncons bs
    if L.null t
        then putBits (end - begin) (h `shiftR` begin)
        else do
            putBits (8 - begin) (h `shiftR` begin)
            putBitString $ BitString t 0 end

putBytes :: Monad m => L.ByteString -> BitPutT m ()
putBytes bytes = bitPutOffset >>= f where
    f 0 = BitPutT $ tell $ fromLazyByteString bytes
    f _ = mapM_ (putBits 8) $ L.unpack bytes
