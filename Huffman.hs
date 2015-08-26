module Huffman
(

) where

import Data.Bits
import Data.List

type LengthSpec = [(Int, Int)]
type Code = [(Int, Int)]

fixedCodeLengthSpec :: LengthSpec
fixedCodeLengthSpec = [
    (8, 144)  -- 0 - 143
  , (9, 112)  -- 144 - 255
  , (7, 24)   -- 256 - 279
  , (8, 8)    -- 280 - 287
  ]

-- Algorithm: Go through the (possibly expanded) list of lengths, and transform
-- it into lists of code values that use each length. Then, compute the codes
-- with appropriate lengths one by one
indexesByLength :: LengthSpec -> [[Int]]
indexesByLength spec = map (fetchAllWithLength spec) [1..(maxLen spec)]
  where maxLen = maximum . map fst
        fetchAllWithLength spec len = elemIndices len (expand spec)

expand :: LengthSpec -> [Int]
expand = concatMap expandPair
  where expandPair (len, count) = replicate count len

specToCode :: LengthSpec -> Code
specToCode spec = buildCode 0 $ indexesByLength spec

buildCode :: Int -> [[Int]] -> Code
buildCode next ((val:vals):lists) = 
  (next, val) : buildCode (next+1) (vals:lists)
buildCode next (_:lists) = 
  buildCode (next `shiftL` 1) lists
buildCode _ _ = []
