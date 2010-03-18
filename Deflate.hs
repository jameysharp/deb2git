module Deflate (
    inflate, reflate
) where

import Bits
import Huffman
import SlidingWindow

import Control.Monad.Identity
import Control.Monad.Trans
import Data.Array.Unboxed
import Data.Binary.Get hiding (getBytes)
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as Foldable
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Word
import GHC.Exts

toDescList :: Foldable.Foldable t => t a -> [a]
{-# INLINE toDescList #-}
toDescList t = build (\ c n -> Foldable.foldl (flip c) n t)

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] b = b
mergeBy _ a [] = a
mergeBy cmp (a:as) (b:bs) = if a `cmp` b /= GT
    then a : mergeBy cmp as (b:bs)
    else b : mergeBy cmp (a:as) bs

lzchoices :: L.ByteString -> [[(Int, Int)]]
lzchoices everything = unfoldr bump (Map.empty, 0, L.replicate 32768 0 `L.append` everything) ++ [[], []] where
    bump (_, _, buffer) | L.null (L.drop 32770 buffer) = Nothing
    bump (history, ringptr, buffer) = Just (matches, (history', narrow (ringptr + 1), L.tail buffer)) where
        narrow n = n .&. 32767
        key = L.take 258 $ L.drop 32768 buffer
        offsetof n = narrow (ringptr - n - 1) + 1
        common a b = length $ takeWhile id $ L.zipWith (==) a b
        lengthof n = common key $ L.drop (fromIntegral $ narrow (n - ringptr)) buffer
        makeRefs s = let l = Foldable.toList s in (lengthof $ head l, map offsetof l)
        (less, exact, more) = Map.splitLookup key history
        lessRefs = map makeRefs $ toDescList less
        moreRefs = map makeRefs $ Foldable.toList more
        mergedRefs = takeWhile ((>= 3) . fst) $ mergeBy (comparing (Down . fst)) lessRefs moreRefs
        mergeDists l = (fst $ head l, foldr (mergeBy compare . snd) [] l)
        inexactRefs = map mergeDists $ groupBy ((==) `on` fst) mergedRefs
        matches = concatMap (uncurry (map . (,))) $ maybe id ((:) . makeRefs) exact inexactRefs
        history' = Map.insertWith (Seq.><) key (Seq.singleton ringptr) $ Map.alter (>>= prune) (L.take 258 buffer) history
        prune xs = case Seq.viewr xs of
            (xs' Seq.:> a) | a == ringptr -> if Seq.null xs' then Nothing else Just xs'
            _ -> Just xs

getCodeLengthSymbols :: BitGet Word8 -> Int -> BitGet [Word8]
getCodeLengthSymbols getLengthSym = next (fail "code lengths begin with repeat") where
    next _ n | n < 0 = fail "more code lengths than expected"
    next _ 0 = return []
    next prev n = do
        sym <- getLengthSym
        case sym of
            16 -> prev >>= rep n 3 2
            17 -> rep n 3 3 0
            18 -> rep n 11 7 0
            _ -> liftM (sym :) $ next (return sym) (n - 1)
    rep n base extra prev = do
        count <- liftM (base +) $ getBits extra
        rest <- next (return prev) (n - count)
        return $ replicate count prev ++ rest

getLZSymbol :: HuffmanTable -> BitGet (Maybe LZSym)
getLZSymbol (litCodec, distCodec) = do
    sym <- getSymbol litCodec
    case sym of
        256 -> return Nothing
        285 -> getdist 258
        _ | sym <= 255 -> return $ Just $ LZLit $ fromIntegral sym
        _ | sym <= 264 -> getdist (fromIntegral sym - 257 + 3)
        _ | sym <= 284 -> do
            let i = fromIntegral sym - 261
            let extrabits = i `shiftR` 2
            let base = bit (extrabits + 2) + 3
            let this = fromIntegral (i .&. 3)
            extra <- getBits extrabits
            getdist $ base + ((this `shiftL` extrabits) .|. extra)
        _ -> fail "unexpected length/literal symbol"
    where
    getdist lit = do
        code <- getSymbol distCodec
        liftM (Just . LZRef lit) $ case code of
            _ | code <= 3 -> return $ fromIntegral code + 1
            _ | code <= 29 -> do
                let i = fromIntegral code - 2
                let extrabits = i `shiftR` 1
                let base = bit (extrabits + 1) + 1
                let this = fromIntegral (i .&. 1)
                extra <- getBits extrabits
                return $ base + ((this `shiftL` extrabits) .|. extra)
            _ -> fail "unexpected distance symbol"

lengthExtraBits :: Int -> Int
lengthExtraBits = fromIntegral . (a!) where
    a :: UArray Int Word8
    a = listArray (3,257) $ concat [replicate (bit (fromIntegral bits + 2)) bits | bits <- 0:[0..5]]

distExtraBits :: Int -> Int
distExtraBits = fromIntegral . idx . subtract 1 where
    idx n = if n < 128 then small ! n else big ! (n `shiftR` 7)
    small :: UArray Int Word8
    small = listArray (0,127) $ concat [replicate (bit (fromIntegral bits + 1)) bits | bits <- 0:[0..5]]
    big :: UArray Int Word8
    big = listArray (1,255) $ concat [replicate (bit (fromIntegral bits - 6)) bits | bits <- [6..13]]

putLZSymbol :: Monad m => HuffmanTable -> Maybe LZSym -> BitPutT m ()
putLZSymbol (litCodec, _) Nothing = putSymbol litCodec 256
putLZSymbol (litCodec, _) (Just (LZLit c)) = putSymbol litCodec $ fromIntegral c
putLZSymbol (litCodec, distCodec) (Just (LZRef len dist)) = do
    if len == 258 then putSymbol litCodec 285 else do
        let lbits = lengthExtraBits len
        let lcode = fromIntegral $ 257 + lbits * 4 + (len - 3) `shiftR` lbits
        putSymbol litCodec lcode
        putBits lbits (len - 3)
    let dbits = distExtraBits dist
    let dcode = fromIntegral $ dbits * 2 + (dist - 1) `shiftR` dbits
    putSymbol distCodec dcode
    putBits dbits (dist - 1)

type HuffmanTable = (HuffmanCodec Word16, HuffmanCodec Word8)
dynamicHuffman :: BitGet HuffmanTable
dynamicHuffman = do
    hlit <- liftM (257 +) $ getBits 5
    hdist <- liftM (1 +) $ getBits 5
    hclen <- liftM (4 +) $ getBits 4
    let order = [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ]
    getLengthSym <- liftM (getSymbol . makeHuffmanCodec . sortBy (comparing fst) . zip order) $ sequence $ replicate hclen (getBits 3)
    lengths <- getCodeLengthSymbols getLengthSym (hlit + hdist)
    let (litlengths, distlengths) = splitAt hlit lengths
    let litCodec = makeHuffmanCodec $ zip [0..] litlengths
    let distCodec = makeHuffmanCodec $ zip [0..] distlengths
    return (litCodec, distCodec)

fixedTable :: HuffmanTable
fixedTable = (makeHuffmanCodec [ (c, l) | (l, cs) <- [(8, [0..143]), (9, [144..255]), (7, [256..279]), (8, [280..287])], c <- cs ],
              makeHuffmanCodec [ (c, 5) | c <- [0..31] ])

data DeflateBlockHeader = UncompressedHeader Word16 | HuffmanHeader (Maybe HuffmanTable)

parseDeflateHeader :: Int -> BitGet (Bool, DeflateBlockHeader)
parseDeflateHeader offset = do
    done <- getBits 1
    kind <- getBits 2
    liftM ((,) done) $ case (kind :: Int) of
        0 -> do
            skipBits $ negate (offset + 3) .&. 7
            liftM UncompressedHeader $ getBits 16
        1 -> return $ HuffmanHeader Nothing
        2 -> liftM (HuffmanHeader . Just) dynamicHuffman
        _ -> fail "deflate block with reserved type"

data LZSym = LZLit {-# UNPACK #-} !Word8 | LZRef {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving Show

data DeflateBlock = Uncompressed S.ByteString | LempelZiv [LZSym]
    deriving Show

parseDeflateBlock :: BitGet (Bool, (BitString, DeflateBlock))
parseDeflateBlock = do
    offset <- bitOffset
    (bits, (done, header)) <- gatherBits $ parseDeflateHeader offset
    block <- case header of
        UncompressedHeader len -> do
            clen <- getBits 16
            unless (len == complement clen) $ fail "bad length in uncompressed block"
            liftM Uncompressed $ getBytes (fromIntegral len)
        HuffmanHeader maybeTable -> do
            let table = fromMaybe fixedTable maybeTable
            let getLZSymbols = do
                    maybesym <- getLZSymbol table
                    case maybesym of
                        Nothing -> return []
                        Just lzsym -> liftM (lzsym :) getLZSymbols
            liftM LempelZiv getLZSymbols
    return (done, (bits, block))

parseDeflateBlocks :: BitGet [(BitString, DeflateBlock)]
parseDeflateBlocks = do
    (done, block) <- parseDeflateBlock
    liftM (block :) $ if done then return [] else parseDeflateBlocks

inflateBlocks :: [DeflateBlock] -> L.ByteString
inflateBlocks = extract . Foldable.foldMap doBlock where
    doBlock (Uncompressed bs) = Foldable.foldMap write $ S.unpack bs
    doBlock (LempelZiv lzs) = Foldable.foldMap unLZ lzs
    unLZ (LZRef len dist) = mconcat $ replicate len $ copy dist
    unLZ (LZLit c) = write c

data Unpredict = ULit | UEOF {-# UNPACK #-} !Int | URef {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving Show

unpredict :: Monad m => [(BitString, DeflateBlock)] -> [[(Int, Int)]] -> BitPutT m ()
unpredict [] [] = return ()
unpredict [] _ = error "unpredict: too many choices"
unpredict ((header, block) : blocks) choices = do
    putBitString header
    offset <- bitPutOffset
    putBits (negate offset .&. 7) (0 :: Int)
    case block of
        Uncompressed bs -> unpredict blocks $ drop (S.length bs) choices
        LempelZiv lzs -> do
            let ((trailing, choices'), unpredicts) = mapAccumL (trailingNothings lzsym) (0, choices) lzs
            mapM_ putUnpredict $ catMaybes unpredicts
            putUnpredict $ UEOF trailing
            unpredict blocks choices'
    where
    lzsym :: [[(Int, Int)]] -> LZSym -> ([[(Int, Int)]], Maybe Unpredict)
    lzsym (cs : css) (LZRef len dist) = (drop (len - 1) css, Just $ badness len dist cs)
    lzsym ([] : css) (LZLit _) = (css, Nothing)
    lzsym (_ : css) (LZLit _) = (css, Just ULit)
    lzsym [] _ = error "unpredict: not enough choices"

    trailingNothings :: (a -> b -> (a, Maybe c)) -> (Int, a) -> b -> ((Int, a), Maybe c)
    trailingNothings f (trailing, a) b = case f a b of
        (a', Nothing) -> let trailing' = trailing + 1 in trailing' `seq` ((trailing', a'), Nothing)
        (a', mc) -> ((0, a'), mc)

    badness len dist = badness' 0 where
        badness' _ [] = error "lzchoices missed a choice"
        badness' l ((len', dist') : xs) | dist == dist' = URef (len' - len) l
                                        | otherwise = badness' (l + 1) xs

putInt :: Monad m => Int -> BitPutT m ()
putInt n | n < 128 = putBits 8 n
putInt n = do
    putBits 8 $ n .|. 0x80
    putInt $ n `shiftR` 7

putUnpredict :: Monad m => Unpredict -> BitPutT m ()
putUnpredict ULit = putInt 1
putUnpredict (URef dLen dDist) = putInt $ ((dLen `shiftL` 15) .|. dDist) `shiftL` 1
putUnpredict (UEOF trailing) = putInt $ (trailing `shiftL` 1) + 3

getInt :: Get Int
getInt = do
    byte <- liftM fromIntegral getWord8
    if testBit byte 7
        then do
            rest <- getInt
            return $ (rest `shiftL` 7) .|. (byte .&. 0x7f)
        else return byte

getUnpredict :: Get Unpredict
getUnpredict = do
    n <- getInt
    return $ case n of
        1 -> ULit
        _ | odd n -> UEOF $ (n `shiftR` 1) - 1
        _ -> URef (n `shiftR` 16) ((n `shiftR` 1) .&. 0x7fff)

inflate :: Get (L.ByteString, L.ByteString)
inflate = do
    blocks <- runBitGet parseDeflateBlocks
    let inflated = inflateBlocks $ map snd blocks
    return (runIdentity $ execBitPutT $ unpredict blocks $ lzchoices inflated, inflated)

reflate :: L.ByteString -> L.ByteString -> L.ByteString
reflate uncompressed = runGet $ execBitPutT (reflate' uncompressed $ lzchoices uncompressed) where
    reflate' bs css = do
        offset <- bitPutOffset
        (bits, (done, header)) <- lift $ runBitGet $ gatherBits $ parseDeflateHeader offset
        putBitString bits
        (bs', css') <- case header of
            UncompressedHeader len -> do
                putBits 16 (complement len)
                let (blockData, bs') = L.splitAt (fromIntegral len) bs
                putBytes blockData
                return (bs', drop (fromIntegral len) css)
            HuffmanHeader maybeTable ->
                lift getUnpredict >>= reflateBlock (fromMaybe fixedTable maybeTable) css bs
        if not done
            then reflate' bs' css'
            else unless (L.null bs' && null css') $ fail $ "reflate: too many choices or too much data: " ++ show (bs', css')

    reflateBlock table css bs (UEOF trailing) = do
        let (lits, rest) = L.splitAt (fromIntegral trailing) bs
        let (skipped, css') = splitAt trailing css
        unless (all null skipped) $ fail "reflate: UEOF skipped a choice"
        mapM_ (putLZSymbol table . Just . LZLit) $ L.unpack lits
        putLZSymbol table Nothing
        return (rest, css')
    reflateBlock table ([] : css) bs u = do
        putLZSymbol table $ Just $ LZLit $ L.head bs
        reflateBlock table css (L.tail bs) u
    reflateBlock table (_ : css) bs ULit = do
        putLZSymbol table $ Just $ LZLit $ L.head bs
        lift getUnpredict >>= reflateBlock table css (L.tail bs)
    reflateBlock table (cs : css) bs (URef dLen dDist) = do
        let (bestLen, dist) = cs !! dDist
        let len = bestLen - dLen
        putLZSymbol table $ Just $ LZRef len dist
        lift getUnpredict >>= reflateBlock table (drop (len-1) css) (L.drop (fromIntegral len) bs)
    reflateBlock _ [] _ _ = fail "reflate: not enough choices"
