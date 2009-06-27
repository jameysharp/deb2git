module SlidingWindow (SlidingWindow, write, copy, extract) where

import Data.Bits
import qualified Data.ByteString as S
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal
import Data.Monoid
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe

newtype SlidingWindow = SlidingWindow ((RingBuffer -> L.ByteString) -> RingBuffer -> L.ByteString)
data RingBuffer = RingBuffer (ForeignPtr Word8) {-# UNPACK #-} !Int
instance Monoid SlidingWindow where
    (SlidingWindow f) `mappend` (SlidingWindow g) = SlidingWindow (f . g)
    mempty = SlidingWindow id

write :: Word8 -> SlidingWindow
write w = SlidingWindow $ \ k (RingBuffer fp idx) -> inlinePerformIO $ do
    withForeignPtr fp $ \ p -> poke (p `plusPtr` idx) w
    let buf' = RingBuffer fp $ (idx + 1) .&. 32767
    return $ case buf' of
        RingBuffer _ 0 -> S.copy (fromForeignPtr fp 0 32768) `chunk` k buf'
        _ -> k buf'
copy :: Int -> SlidingWindow
copy dist = SlidingWindow $ \ k buf@(RingBuffer fp idx) -> inlinePerformIO $ do
        w <- withForeignPtr fp $ \ p -> peek (p `plusPtr` ((idx - dist) .&. 32767))
        let (SlidingWindow k') = write w
        return $ k' k buf
extract :: SlidingWindow -> L.ByteString
extract (SlidingWindow k) = k finish newRing where
    newRing = RingBuffer (unsafePerformIO $ mallocByteString 32768) 0
    finish (RingBuffer fp len) = (fromForeignPtr fp 0 len) `chunk` L.empty
