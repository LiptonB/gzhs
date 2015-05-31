module BitGet
( BitGet
, getBit
, getWord8
, getAlignedWord16le
, getAlignedLazyByteString
, skipToNextByte
, runBitGet
) where
import Data.Bits
import Data.Word
import qualified Data.Binary.Get as G
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as L

-- newtype BitGet a = B { runBitGet :: StateT S Get a }
type BitGet  = StateT S G.Get
data S = S {
        position :: Int
      , buffer :: Word8
      , buffer2 :: Word8
      }

getBit :: BitGet Bool
getBit = do
  refreshBuffer
  s <- get
  modify $ advance 1
  return $ readBit s

advance :: Int -> S -> S
advance n s@(S pos _ _) = s { position = pos+n }

readBit :: S -> Bool
readBit (S pos buf _) =
  testBit buf (7-pos)

getWord8 :: Int -> BitGet Word8
getWord8 n = do
  refreshBuffer
  s <- get
  modify $ advance n
  return $ readWord8 n s

getAlignedWord16le :: BitGet Word16
getAlignedWord16le = do
  skipToNextByte
  b1 <- getWord8 8
  b2 <- getWord8 8
  return (fromIntegral b2 `shiftL` 8 .|. fromIntegral b1)

getAlignedLazyByteString :: Int -> BitGet L.ByteString
getAlignedLazyByteString len = do
  skipToNextByte
  refreshBuffer
  S _ buf buf2 <- get
  modify $ advance 16
  rest <- lift $ G.getLazyByteString (fromIntegral len - 2)
  return $ buf `L.cons` buf2 `L.cons` rest

readWord8 :: Int -> S -> Word8
readWord8 n (S pos buf buf2)
  = word8FromBufs n pos buf buf2

word8FromBufs n pos buf buf2
  | n <= 8 = fromIntegral $ (buf `shiftR` pos .|. buf2 `shiftL` (8 - pos)) .&. mask n
  | otherwise = error "n must be less than 8"
      where mask n = (1 `shiftL` n) - 1

skipToNextByte :: BitGet ()
skipToNextByte = modify skip
                 where skip s = s { position = newpos s }
                       newpos s
                        | position s == 0 = 0
                        | otherwise = 8

refreshBuffer :: BitGet ()
refreshBuffer = do
  S pos buf buf2 <- get
  if pos >= 8
  then do
    w <- lift G.getWord8
    put $ S (pos-8) buf2 w
    refreshBuffer
  else return ()

runBitGet :: BitGet a -> G.Get a
runBitGet bg = evalStateT bg initState

initState = S 16 0 0
