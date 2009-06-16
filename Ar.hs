module Ar (readAr, readArTree) where

import Tree

import Control.Monad
import Data.Binary.Get
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

readEntries :: Get [(String, AbstractMeta L.ByteString, L.ByteString)]
readEntries = do
    empty <- isEmpty
    if empty then return [] else do
    entry <- readEntry
    rest <- readEntries
    return $ entry : rest

readEntry :: Get (String, AbstractMeta L.ByteString, L.ByteString)
readEntry = do
    name <- rtrim 16
    mtime <- rtrim 12; getInt (10 :: Int) mtime
    uid <- rtrim 6; getInt (10 :: Int) uid
    gid <- rtrim 6; getInt (10 :: Int) gid
    mode <- rtrim 8; getInt (8 :: Int) mode
    len <- rtrim 10 >>= getInt 10
    match arFileMagic
    body <- getLazyByteString len
    when (odd len) $ match (S.singleton '\n')
    return (L.unpack name, [("mtime", mtime), ("uid", uid), ("gid", gid), ("mode", mode)], body)

readArTree :: L.ByteString -> AbstractTree L.ByteString
readArTree = archiveToTree mktree . readAr where
    mktree (_, meta, bytes) = case lookup "mode" meta >>= getInt (8 :: Int) of
        Just 0o040000 | L.null bytes -> Tree []
        _ -> Blob bytes

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

getInt :: (Monad m, Num a) => a -> L.ByteString -> m a
getInt base bytes = case readInt base val digitToInt $ L.unpack bytes of
    [(n, "")] -> return n
    _ -> fail $ "invalid number in base " ++ show base ++ ": " ++ L.unpack bytes
    where val = case base of 8 -> isOctDigit; 10 -> isDigit; _ -> error "unsupported base"
