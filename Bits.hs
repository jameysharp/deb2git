module Bits (
    BitGet, runBitGet, bitOffset, getBits, skipBits, getBytes,
    BitString, gatherBits,
    BitPutT, runBitPutT, execBitPutT, bitPutOffset, putBits, putBitString, putBytes
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Binary.Builder
import Data.Binary.Get hiding (getBytes)
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word

data S = S {-# UNPACK #-} !Int {-# UNPACK #-} !Word8
type BitGet a = StateT S Get a
data BitString = BitString L.ByteString !Int !Int

runBitGet :: BitGet a -> Get a
runBitGet = flip evalStateT $ S 0 0

bitOffset :: BitGet Int
bitOffset = do
    S avail byte <- get
    return $ negate avail .&. (bitSize byte - 1)

getBits :: Enum e => Int -> BitGet e
getBits = liftM toEnum . (get >>=) . align where
    align count (S avail byte)
        | count <= avail = put (S (avail - count) byte) >> return (bits .&. mask)
        | otherwise = do
            byte' <- lift getWord8
            v <- align (count - avail) (S (bitSize byte') byte')
            return $ (v `shiftL` avail) .|. bits
        where
        mask = bit count - 1
        used = bitSize byte - avail
        bits = fromEnum (byte `shiftR` used)

skipBits :: Int -> BitGet ()
skipBits n = do
    getBits n :: BitGet Int
    return ()

getBytes :: Int -> BitGet S.ByteString
getBytes count = do
    S avail _ <- get
    when (avail /= 0) $ fail "getBytes: input stream not byte-aligned"
    lift $ getByteString count

gatherBits :: BitGet a -> BitGet (BitString, a)
gatherBits m = do
    str <- lift getRemainingLazyByteString
    bytes <- lift bytesRead
    S avail byte <- get
    let add s = if avail /= 0 then L.cons byte s else s
    let begin = negate avail .&. (bitSize byte - 1)
    a <- m
    bytes' <- lift bytesRead
    end <- bitOffset
    return (BitString (add $ L.take (bytes' - bytes) str) begin end, a)

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
