module Huffman
(

) where

import qualified Data.Map as Map

type LengthSpec = [(Int, Int)]

fixedCodeLengthSpec :: LengthSpec
fixedCodeLengthSpec =
  [ (8, 144)  -- 0 - 143
  , (9, 112)  -- 144 - 255
  , (7, 24)   -- 256 - 279
  , (8, 8)    -- 280 - 287
  ]

countSpecLengths :: LengthSpec -> [Int]
countSpecLengths spec =
  accumLengths spec (maxLen spec) []
  where accumLengths spec 0 accum = accum
        accumLengths spec len accum =
          accumLengths spec (len-1) ((getCount len spec):accum)
        getCount len = sum . map snd . filter ((== len) . fst)
        maxLen = maximum . map fst

--codeFromLengths :: [Int] -> Code
--codeFromLengths lengths =

-- IDEA: Go through the (possibly expanded) list of lengths, and transform it
-- into lists of code values that use each length. Then, we can easily assign
-- codes to those values once we compute the minimum code for each length. In
-- fact, we could probably compute the codes as we go as long as we sort the
-- value lists by code length before starting to assign.
