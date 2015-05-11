import Text.Parsec.ByteString.Lazy
import Control.Applicative ((<*), (*>))
import Data.Char (chr)
import Data.Word
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

sample :: L.ByteString
sample = L.pack $ [31, 139, 8, 0] ++ toBytes 4 mtime ++ [4, 3]

mtime :: Int
mtime = 1431284945

toBytes :: Int -> Int -> [Word8]
toBytes 0 _ = []
toBytes n v = fromIntegral (rem v 256) : toBytes (n-1) (div v 256)

fromBytes :: [Word8] -> Int
fromBytes [] = 0
fromBytes (w:ws) = fromIntegral w + 256 * fromBytes ws

parseHeader :: L.ByteString -> Either String (Word8, Int, Word8, Word8)
parseHeader s =
    case L.splitAt 3 s of
      (ids, t) | ids /= L.pack [31, 139, 8] -> Left "Incorrect ID field"
               | otherwise ->
                  case L.uncons t of
                    Nothing -> Left "No flags"
                    Just (flags, t) ->
                        let (mtime, r) = L.splitAt 4 t in
                            case L.uncons r of
                              Nothing -> Left "No mtime"
                              Just (xfl, r) ->
                                  case L.uncons r of
                                    Nothing -> Left "No os"
                                    Just (os, r) ->
                                        Right (flags, (fromBytes . L.unpack) mtime, xfl, os)
