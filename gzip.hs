import Control.Applicative ((<*), (*>))
import Data.Char (chr)
import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as L

-- type GZipFile = [Member]
-- data GZipHeader = GZipHeader {
--       flags :: [GZipFlag]
--     , mtime :: Int
--     , xlen :: Int
--     , crc32 :: [Word8]
--     , isize :: Int
--     }
-- gzipFile :: GenParser Char st [String]
-- gzipFile = many member <* eof

-- member :: GenParser Char st String
-- member = magic *> method

-- magic :: GenParser Word8 st [Word8]
-- magic = string $ map chr [31,139]

-- method :: GenParser Word8 st Char
-- method = char $ chr 8

sample,sample2,sample3 :: L.ByteString
sample = L.pack $ [31, 139, 8, 0] ++ toBytes 4 mtime ++ [4, 3]
sample2 = L.pack $ [31, 139, 6, 0] ++ toBytes 4 mtime ++ [4, 3]
sample3 = L.pack $ [31, 139, 8, 0] ++ toBytes 4 mtime

mtime :: Int
mtime = 1431284945

toBytes :: Int -> Int -> [Word8]
toBytes 0 _ = []
toBytes n v = fromIntegral (rem v 256) : toBytes (n-1) (div v 256)

fromBytes :: [Word8] -> Int
fromBytes [] = 0
fromBytes (w:ws) = fromIntegral w + 256 * fromBytes ws

parseHeaderGet :: Get (Word8, Int, Word8, Word8)
parseHeaderGet = do
    ids <- getLazyByteString 3
    flags <- getWord8
    mtime <- getLazyByteString 4
    xfl <- getWord8
    os <- getWord8
    if ids == L.pack [31, 139, 8]
        then return (flags, (fromBytes . L.unpack) mtime, xfl, os)
        else fail "Incorrect IDs"
