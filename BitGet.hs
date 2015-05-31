module BitGet
( BitGet
, getBit
, getWord8
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

readWord8 :: Int -> S -> Word8
readWord8 n (S pos buf buf2)
  | n == 0 = 0
  | n <= 8 = word8FromBufs n pos buf buf2
  | otherwise = error "n must be less than 8"

word8FromBufs n pos buf buf2 =
  fromIntegral $ (val1 `shiftR` pos .|. val2 `shiftL` (8 - pos)) .&. mask n
  where val1 = fromIntegral buf :: Word16
        val2 = fromIntegral buf2 :: Word16
        mask n = (1 `shiftL` n) - 1

skipToNextByte :: BitGet ()
skipToNextByte = modify skip
                 where skip s = s { position = 8 }

refreshBuffer :: BitGet ()
refreshBuffer = do
  S pos buf buf2 <- get
  if pos >= 8
  then do
    w <- lift G.getWord8
    put $ S (pos-8) buf2 w
  else return ()

runBitGet :: BitGet a -> G.Get a
runBitGet bg = evalStateT bg initState

initState = S 8 0 0
