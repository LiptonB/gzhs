module Huffman
(
  getCodePoint
) where

import Data.Bits
import Data.List
import qualified BitGet as BG

-- Ordered list where each pair (a, b) signifies that the next b codes have
-- length a
type LengthSpec = [(Int, Int)]
-- Map from code point to translated value
type Code = [(Int, Int)]
-- A CodeTree is a tree with the value of the code point at the leaf
data CodeTree = Branch CodeTree CodeTree | Leaf Int | Empty
  deriving Show


fixedCodeLengthSpec :: LengthSpec
fixedCodeLengthSpec = [
    (8, 144)  -- 0 - 143
  , (9, 112)  -- 144 - 255
  , (7, 24)   -- 256 - 279
  , (8, 8)    -- 280 - 287
  ]

-- Algorithm: Go through the expanded list of lengths, and transform it into
-- lists of code values that use each length. Then, compute the codes with
-- appropriate lengths one by one
indexesByLength :: LengthSpec -> [[Int]]
indexesByLength spec = map (fetchAllWithLength spec) [0..(maxLen spec)]
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
buildCode next ([]:lists) = 
  buildCode (next `shiftL` 1) lists
buildCode _ [] = []

-- TODO: explain output (maybe that will make me understand it)
buildCodeTree :: [[Int]] -> (CodeTree, [[Int]])
--buildCodeTree [] = Empty  -- TODO: is this needed?
buildCodeTree ((val:vals):lists) = ((Leaf val), vals:lists)
buildCodeTree ([]:lists) = let
  (left, lists') = buildCodeTree lists
  (right, lists'') = buildCodeTree lists'
  in ((Branch left right), []:lists'')

getCodePoint :: CodeTree -> BG.BitGet Int
getCodePoint code =
  case code of
    Leaf val -> return val
    Branch left right -> do
      nextBit <- BG.getBit
      if nextBit
      then getCodePoint right
      else getCodePoint left
    Empty -> error "Unknown code"
