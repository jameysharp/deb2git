import Bits
import Huffman

import Control.Monad.State
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Foldable as Foldable
import Data.List
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Ord
import qualified Data.Sequence as Seq
import Data.Word
import GHC.Exts
import System.Environment

lzchoices :: L.ByteString -> [[(Int, Int)]]
lzchoices everything = unfoldr bump (IntMap.empty, 0, L.replicate 32768 0 `L.append` everything) where
    bump (_, _, buffer) | L.null (L.drop 32770 buffer) = Nothing
    bump (history, ringptr, buffer) = Just (matches, (history', narrow (ringptr + 1), L.tail buffer)) where
        narrow n = n .&. 32767
        input = L.drop 32768 buffer
        -- XXX: keep previous key and update it incrementally
        key3 bs = let b = fromIntegral . L.index bs in (b 0 `shiftL` 16) .|. (b 1 `shiftL` 8) .|. b 2
        key = key3 input
        offsetof n = narrow (ringptr - n - 1) + 1
        common a b = length $ takeWhile id $ L.zipWith (==) a b
        lengthof n = common (L.take 258 input) $ L.drop (fromIntegral $ narrow (n - ringptr)) buffer
        matches = [ (lengthof n, offsetof n) | n <- Foldable.toList $ IntMap.findWithDefault Seq.empty key history ]
        history' = IntMap.insertWith (Seq.><) key (Seq.singleton ringptr) $ IntMap.alter (>>= prune) (key3 buffer) history
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

getLZSymbol :: HuffmanTable -> BitGet LZSym
getLZSymbol (getLitSym, getDistSym) = do
    sym <- getLitSym
    case sym of
        256 -> return LZEOF
        285 -> getdist 258
        _ | sym <= 255 -> return $ LZLit $ fromIntegral sym
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
        code <- getDistSym
        liftM (LZRef lit) $ case code of
            _ | code <= 3 -> return $ fromIntegral code + 1
            _ | code <= 29 -> do
                let i = fromIntegral code - 2
                let extrabits = i `shiftR` 1
                let base = bit (extrabits + 1) + 1
                let this = fromIntegral (i .&. 1)
                extra <- getBits extrabits
                return $ base + ((this `shiftL` extrabits) .|. extra)
            _ -> fail "unexpected distance symbol"

type HuffmanTable = (BitGet Word16, BitGet Word8)
dynamicHuffman :: BitGet HuffmanTable
dynamicHuffman = do
    hlit <- liftM (257 +) $ getBits 5
    hdist <- liftM (1 +) $ getBits 5
    hclen <- liftM (4 +) $ getBits 4
    let order = [ 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 ]
    getLengthSym <- liftM (makeHuffmanDecoder . sortBy (comparing fst) . zip order) $ sequence $ replicate hclen (getBits 3)
    lengths <- getCodeLengthSymbols getLengthSym (hlit + hdist)
    let (litlengths, distlengths) = splitAt hlit lengths
    let getLitSym = makeHuffmanDecoder $ zip [0..] litlengths
    let getDistSym = makeHuffmanDecoder $ zip [0..] distlengths
    return (getLitSym, getDistSym)

fixedTable :: HuffmanTable
fixedTable = (makeHuffmanDecoder [ (c, l) | (l, cs) <- [(7, [256..279]), (8, [0..143] ++ [280..287]), (9, [144..255])], c <- cs ],
              makeHuffmanDecoder [ (c, 5) | c <- [0..31] ])

data DeflateBlockHeader = UncompressedHeader Word16 | HuffmanHeader (Maybe HuffmanTable)

parseDeflateHeader :: BitGet (Bool, DeflateBlockHeader)
parseDeflateHeader = do
    done <- getBits 1
    kind <- getBits 2
    liftM ((,) done) $ case (kind :: Int) of
        0 -> do
            byteAlign
            liftM UncompressedHeader $ getBits 16
        1 -> return $ HuffmanHeader Nothing
        2 -> liftM (HuffmanHeader . Just) dynamicHuffman
        _ -> fail "deflate block with reserved type"

data LZSym = LZLit {-# UNPACK #-} !Word8 | LZEOF | LZRef {-# UNPACK #-} !Int {-# UNPACK #-} !Int
    deriving Show

data DeflateBlock = Uncompressed S.ByteString | LempelZiv [LZSym]
    deriving Show

parseDeflateBlock :: BitGet (Bool, DeflateBlock)
parseDeflateBlock = do
    (done, header) <- parseDeflateHeader
    liftM ((,) done) $ case header of
        UncompressedHeader len -> do
            clen <- getBits 16
            unless (len == complement clen) $ fail "bad length in uncompressed block"
            liftM Uncompressed $ getBytes (fromIntegral len)
        HuffmanHeader maybeTable -> do
            let table = fromMaybe fixedTable maybeTable
            let getLZSymbols = do
                    lzsym <- getLZSymbol table
                    case lzsym of
                        LZEOF -> return []
                        _ -> liftM (lzsym :) getLZSymbols
            liftM LempelZiv getLZSymbols

parseDeflateBlocks :: BitGet [DeflateBlock]
parseDeflateBlocks = do
    (done, block) <- parseDeflateBlock
    if done
        then return [block]
        else liftM (block :) parseDeflateBlocks

type SlidingWindow a = State [Word8] a

write :: Word8 -> SlidingWindow ()
write w = modify (w :)
copy :: Int -> SlidingWindow ()
copy dist = do
    bs <- get
    write $ bs !! (dist - 1)
extract :: SlidingWindow () -> L.ByteString
extract = L.pack . reverse . flip execState []

inflateBlocks :: [DeflateBlock] -> L.ByteString
inflateBlocks = extract . mapM_ doBlock where
    doBlock (Uncompressed bs) = mapM_ write $ S.unpack bs
    doBlock (LempelZiv lzs) = mapM_ unLZ lzs
    unLZ (LZRef len dist) = replicateM_ len $ copy dist
    unLZ (LZLit c) = write c
    unLZ LZEOF = return ()

unpredict :: [DeflateBlock] -> [[(Int, Int)]] -> [(Int, Int)]
unpredict [] [] = []
unpredict [] _ = error "unpredict: too many choices"
unpredict (Uncompressed bs : blocks) choices = unpredict blocks $ drop (S.length bs) choices
unpredict (LempelZiv lzs : blocks) choices = lzblock lzs choices $ unpredict blocks where
    lzblock :: [LZSym] -> [[(Int, Int)]] -> ([[(Int, Int)]] -> [(Int, Int)]) -> [(Int, Int)]
    lzblock (LZRef len dist : syms) (cs : css) f = badness len dist cs : lzblock syms (drop (len - 1) css) f
    lzblock (LZLit _ : syms) ([] : css) f = lzblock syms css f
    lzblock (LZLit _ : syms) (cs : css) f = (length cs, 0) : lzblock syms css f
    lzblock _ css f = f css

    badness len dist = badness' 0 . sortBy (comparing (Down . fst)) where
        badness' _ [] = error "lzchoices missed a choice"
        badness' l ((len', dist') : xs) | dist == dist' = (l, len' - len)
                                        | otherwise = badness' (l + 1) xs

main :: IO ()
main = do
    args <- getArgs
    flip mapM_ args $ \ name -> do
        d <- L.readFile name
        let d' = L.drop 10 d
        let d'' = if ((d `L.index` 3) .&. 0x8) == 0 then d' else L.drop (1 + fromJust (L.elemIndex 0 d')) d'
        let blocks = runBitGet parseDeflateBlocks d''
        let choices = lzchoices $ inflateBlocks blocks
        mapM_ print $ unpredict blocks choices
