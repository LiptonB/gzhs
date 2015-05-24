import Data.Bits
import Data.Word
import Data.Binary.Get
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as L

-- newtype BitGet a = B { runBitGet :: StateT S Get a }
type BitGet  = StateT S Get
data S = S {
        position :: Int
      , buffer :: Word8
      }

getBit :: BitGet Bool
getBit = do
  refreshBuffer
  s <- get
  let b = testBit (buffer s) (position s)
      s' = s { position = (position s) + 1 }
  put s'
  return b

skipToNextByte :: BitGet ()
skipToNextByte = modify skip
                 where skip s = s { position = 8 }

refreshBuffer :: BitGet ()
refreshBuffer = do
  s <- get
  if position s >= 8
  then do
    w <- lift getWord8
    put $ S 0 w
  else return ()

runBitGet :: BitGet a -> Get a
runBitGet bg = evalStateT bg initState

initState = S 8 0
