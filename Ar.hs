module Ar (readAr, writeAr) where

import Tree

import Control.Monad
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Char
import Data.Int
import Data.List
import Numeric

arMagic, arFileMagic :: S.ByteString
arMagic = S.pack "!<arch>\n"
arFileMagic = S.pack "`\n"

readAr :: L.ByteString -> Archive L.ByteString
readAr = runGet $ do
    match arMagic
    liftM Archive readEntries

writeAr :: Archive L.ByteString -> L.ByteString
writeAr (Archive entries) = runPut $ do
    putByteString arMagic
    mapM_ writeEntry entries

readEntries :: Get [(String, AbstractMeta L.ByteString, Maybe L.ByteString)]
readEntries = do
    empty <- isEmpty
    if empty then return [] else do
    entry <- readEntry
    rest <- readEntries
    return $ entry : rest

readEntry :: Get (String, AbstractMeta L.ByteString, Maybe L.ByteString)
readEntry = do
    name <- rtrim 16
    mtime <- rtrim 12; getInt (10 :: Int) mtime
    uid <- rtrim 6; getInt (10 :: Int) uid
    gid <- rtrim 6; getInt (10 :: Int) gid
    mode <- rtrim 8
    modeint <- getInt (8 :: Int) mode
    len <- rtrim 10 >>= getInt 10
    match arFileMagic
    body <- getLazyByteString len
    when (odd len) $ match (S.singleton '\n')
    return (L.unpack name,
        [("mtime", mtime), ("uid", uid), ("gid", gid), ("mode", mode)],
        if modeint == 0o040000 && L.null body then Nothing else Just body)

data ArMeta = ArMeta { armtime, aruid, argid, armode, arlength :: L.ByteString }

defaultmeta :: Int64 -> ArMeta
defaultmeta len = ArMeta {
    armtime = L.pack "0",
    aruid = L.pack "0",
    argid = L.pack "0",
    armode = L.pack "100644",
    arlength = L.pack $ show len
}

matchmeta :: (String, L.ByteString) -> ArMeta -> ArMeta
matchmeta ("mtime", v) meta = meta { armtime = v }
matchmeta ("uid", v) meta = meta { aruid = v }
matchmeta ("gid", v) meta = meta { argid = v }
matchmeta ("mode", v) meta = meta { armode = v }
matchmeta ("length", v) meta = meta { arlength = v }
matchmeta _ meta = meta

writeEntry :: (String, AbstractMeta L.ByteString, Maybe L.ByteString) -> Put
writeEntry (name, meta, body) = do
    rpad 16 $ L.pack name
    let armeta = foldr matchmeta (defaultmeta $ maybe 0 L.length body) meta
    rpad 12 $ armtime armeta
    rpad 6 $ aruid armeta
    rpad 6 $ argid armeta
    rpad 8 $ armode armeta
    rpad 10 $ arlength armeta
    let len = digitToInt $ L.last $ arlength armeta
    putByteString arFileMagic
    case body of
        Just bytes -> do
            putLazyByteString bytes
            when (odd len) $ putByteString (S.singleton '\n')
        _ -> return ()

match :: S.ByteString -> Get ()
match expected = do
    got <- getByteString $ S.length expected
    when (expected /= got) $ fail $ "expected \"" ++ show expected ++ "\" but got \"" ++ show got ++ "\""

rtrim :: Int -> Get L.ByteString
rtrim rawlen = do
    bytes <- getByteString rawlen
    let len ' ' 0 = 0
        len _ n = n + 1
    return $ L.fromChunks [S.take (S.foldr len 0 bytes) bytes]

rpad :: Int -> L.ByteString -> Put
rpad rawlen s = do
    let (field, excess) = L.splitAt (fromIntegral rawlen) s
    unless (L.null excess) $ fail ("string too long for " ++ show rawlen ++ "-character field: \"" ++ show s ++ "\"")
    putLazyByteString field
    putByteString $ S.replicate (rawlen - fromIntegral (L.length field)) ' '

getInt :: (Monad m, Num a) => a -> L.ByteString -> m a
getInt base bytes = case readInt base val digitToInt $ L.unpack bytes of
    [(n, "")] -> return n
    _ -> fail $ "invalid number in base " ++ show base ++ ": " ++ L.unpack bytes
    where val = case base of 8 -> isOctDigit; 10 -> isDigit; _ -> error "unsupported base"
