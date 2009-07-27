module Gzip (readGzip) where

import Deflate

import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

gzipMagic :: S.ByteString
gzipMagic = S.pack [0x1f, 0x8b, 0x08]

readGzip :: Get (L.ByteString, L.ByteString)
readGzip = do
    match gzipMagic

    -- FIXME save header
    flg <- getWord8
    when ((flg .&. 0xE0) /= 0) $ fail "Gzip: reserved flags are set"
    skip 6
    when (testBit flg 2) $ do
        xlen <- getWord16le
        skip $ fromIntegral xlen
    when (testBit flg 3) $ getLazyByteStringNul >> return ()
    when (testBit flg 4) $ getLazyByteStringNul >> return ()

    -- FIXME check and discard header CRC
    when (testBit flg 1) $ skip 2

    inflate
    -- FIXME read and check crc and length

match :: S.ByteString -> Get ()
match expected = do
    got <- getByteString $ S.length expected
    when (expected /= got) $ fail $ "expected \"" ++ show expected ++ "\" but got \"" ++ show got ++ "\""
