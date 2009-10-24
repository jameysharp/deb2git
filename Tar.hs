module Tar where

import Tree

import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import Data.Int
import Data.Word
import Numeric

readTar :: L.ByteString -> Archive L.ByteString
readTar = Archive . runGet readEntries

readEntries :: Get [(String, AbstractMeta L.ByteString, Maybe L.ByteString)]
readEntries = do
    empty <- isEmpty
    if empty then return [] else do
    header <- getByteString 512
    if S.head header == 0 then return [] else do
    let (name, meta, maybeSize) = readEntry header
    body <- case maybeSize of
	Nothing -> return Nothing
	Just size -> do
	    body <- getLazyByteString size
	    skip $ fromIntegral (negate size .&. 511)
	    return $ Just body
    rest <- readEntries
    return $ (name, meta, body) : rest

readEntry :: S.ByteString -> (String, AbstractMeta L.ByteString, Maybe Int64)
readEntry header = flip runGet (L.fromChunks [header]) $ do
    name <- rtrim 100
    mode <- rtrim 8; getInt (8 :: Int) mode
    uid <- rtrim 8; getInt (8 :: Int) uid
    gid <- rtrim 8; getInt (8 :: Int) gid
    size <- rtrim 12 >>= getInt 8
    mtime <- rtrim 12; getInt (8 :: Int) mtime
    checksum <- rtrim 8 >>= getInt 8
    typeflag <- getWord8
    linkname <- rtrim 100
    let correctChecksum = S.foldl accumChecksum 0 header - sum [fromIntegral $ S.index header x | x <- [148..155]] + 0x20 * 8
    unless (checksum == correctChecksum) $ fail $ "saw checksum " ++ show checksum ++ ", expected " ++ show correctChecksum
    let typechar = toEnum $ fromEnum typeflag
    unless (typechar `elem` "\NUL0125KL") $ fail $ "unsupported typeflag " ++ show typechar
    when (typechar == '1' && size /= 0) $ fail $ "hardlink with size " ++ show size
    let datasize = if typechar `elem` "\NUL0KL" then size else 0
    return (L.unpack name,
        [("typeflag", L.singleton typechar), ("mtime", mtime), ("uid", uid), ("gid", gid), ("mode", mode)],
        if typechar == '5' then Nothing else Just datasize)
  where
    accumChecksum :: Word -> Word8 -> Word
    accumChecksum a c = a + fromIntegral c

rtrim :: Int -> Get L.ByteString
rtrim rawlen = do
    bytes <- getByteString rawlen
    return $ L.fromChunks [S.takeWhile (/= 0) bytes]

getInt :: (Monad m, Num a) => a -> L.ByteString -> m a
getInt base bytes = case readInt base val digitToInt $ L.unpack bytes of
    [(n, "")] -> return n
    _ -> fail $ "invalid number in base " ++ show base ++ ": " ++ show (L.unpack bytes)
    where val = case base of 8 -> isOctDigit; 10 -> isDigit; _ -> error "unsupported base"
