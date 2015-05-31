import Control.Applicative
import Control.Monad
import Data.Char (chr)
import Data.Word
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as L
import qualified BitGet as BG

-- type GZipFile = [Member]
data GZipHeader = GZipHeader {
      gzhFlags :: Word8
    , gzhMtime :: Int
    , gzhXfl :: Word8
    , gzhOs :: Word8
    , gzhText :: Bool
    , gzhExtra :: Maybe L.ByteString
    , gzhName :: Maybe L.ByteString
    , gzhComment :: Maybe L.ByteString
    , gzhCrc :: Maybe L.ByteString
    }
    deriving (Show)

data GZipFlag = FTEXT | FEXTRA | FNAME | FCOMMENT | FHCRC

data BType = Uncompressed | Fixed | Dynamic
-- gzipFile :: GenParser Char st [String]
-- gzipFile = many member <* eof

-- member :: GenParser Char st String
-- member = magic *> method

-- magic :: GenParser Word8 st [Word8]
-- magic = string $ map chr [31,139]

-- method :: GenParser Word8 st Char
-- method = char $ chr 8

sample = L.pack $ [31, 139, 8, 0] ++ toBytes 4 mtime ++ [4, 3]
sampleUncompressedBlock = L.pack $ [0x01, 5, 0, 0xff-5, 0xff-0, 10, 11, 12, 13, 14, 15]

mtime :: Int
mtime = 1431284945

toBytes :: Int -> Int -> [Word8]
toBytes 0 _ = []
toBytes n v = fromIntegral (rem v 256) : toBytes (n-1) (div v 256)

fromBytes :: [Word8] -> Int
fromBytes [] = 0
fromBytes (w:ws) = fromIntegral w + 256 * fromBytes ws

parseHeader :: Get GZipHeader
parseHeader = do
    header <- checkIds *> getHeaderVals
    header' <- if hasFlag FEXTRA header
               then parseExtra header else return header
    header'' <- if hasFlag FNAME header'
                then parseName header' else return header'
    header''' <- if hasFlag FCOMMENT header''
                 then parseComment header'' else return header''
    if hasFlag FHCRC header''' then parseCrc header''' else return header'''

testFlag :: Bits a => GZipFlag -> a -> Bool
testFlag flag = flip testBit (flagToBit flag)

hasFlag :: GZipFlag -> GZipHeader -> Bool
hasFlag flag = testFlag flag . gzhFlags

flagToBit FTEXT = 0
flagToBit FHCRC = 1
flagToBit FEXTRA = 2
flagToBit FNAME = 3
flagToBit FCOMMENT = 4

parseExtra header = do
  xlen <- (fromBytes . L.unpack) <$> getLazyByteString 2
  extra <- getLazyByteString (fromIntegral xlen)
  return header { gzhExtra = Just extra }

parseName header = do
  name <- getLazyByteStringNul
  return header { gzhName = Just name }

parseComment header = do
  comment <- getLazyByteStringNul
  return header { gzhComment = Just comment }

parseCrc header = do
  crc <- getLazyByteString 2
  return header { gzhCrc = Just crc }

checkIds :: Get ()
checkIds = do
  ids <- getLazyByteString 3
  if ids == L.pack [31, 139, 8]
    then return ()
    else fail "Incorrect IDs"

getHeaderVals :: Get GZipHeader
getHeaderVals = do
  flags <- getWord8
  mtime <- getMtime
  xfl <- getWord8
  os <- getWord8
  let text = testFlag FTEXT flags
  return $ GZipHeader flags mtime xfl os text Nothing Nothing Nothing Nothing

getMtime :: Get Int
getMtime = (fromBytes . L.unpack) <$> getLazyByteString 4

parseBlocks :: BG.BitGet L.ByteString
parseBlocks = do
  (v, final) <- parseBlock
  if final
  then return v
  else liftM (L.append v) parseBlocks

btype :: Word8 -> BType
btype 0 = Uncompressed
btype 1 = Fixed
btype 2 = Dynamic

parseBlock :: BG.BitGet (L.ByteString -- uncompressed data of block
                        , Bool)       -- whether block is final
parseBlock = do
  final <- BG.getBit
  bt <- BG.getWord8 2
  bytes <- case btype bt of
    Uncompressed -> parseUncompressed
    Fixed -> parseFixed
    Dynamic -> parseDynamic
  return (bytes, final)

parseUncompressed :: BG.BitGet L.ByteString
parseUncompressed = do
  BG.skipToNextByte
  len <- BG.getAlignedWord16le
  nlen <- BG.getAlignedWord16le
  BG.getAlignedLazyByteString (fromIntegral len)

parseFixed = return (L.pack [])
parseDynamic = return (L.pack [])
