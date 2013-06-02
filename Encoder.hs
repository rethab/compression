module Encoder where

import Control.Arrow       (first, second)
import Control.Monad       (mplus, liftM2)
import Data.Hashable
import Data.Monoid         (mappend)
import Data.Word           (Word32)

import qualified Data.Binary.BitBuilder    as B
import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.Put           as BinPut
import qualified Data.Binary.Strict.Get    as BinGet
import qualified Data.ByteString           as S
import qualified Data.ByteString.Lazy      as L
import qualified Data.HashMap.Strict       as M

data Tree a = Leaf a 
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf v) = Leaf (f v)
    fmap f (Node r l) = Node (fmap f r) (fmap f l)

-- | These bits identify the strucutre of the bit-encoded Huffmann Tree.
--   
--   Starting from left to right:
--    1. read one bit.
--     a. one:  push not to stack, traverse left. goto 1
--     b. zero: leaf. is stack empty?
--       I. yes: done
--       II. no: pop node from stack. traverse right. goto 1
type StrucutreBits = [Bool]

data HuffTree a = HuffTree (M.HashMap a BitBuilder) StructureBits

class Serializable a where
    serialize :: a -> S.ByteString

instance Serializable StructureBits where
    serialize = toBS . foldl mappend B.empty . map B.singleton
  where toBS = L.toStrict . B.toLazyByteString

instance Serializable Word32 where
     serialize  = L.toStrict . BinPut.runPut . BinPut.putWord32be 


-- | Searches a the bits for a value
lookupBits :: HuffTree a -> a -> Maybe BitBuilder
lookupBits (HuffTree map _) needle = M.lookup needle map

-- | Creates a Huffmann Tree from a List of Values and their probablities
createHuffTree :: (Eq a, Hashable a) => [(a, Float)] -> HuffTree a
createHuffTree vals = let tree = (assigneCodes . combineAll . maptoLeaf) vals
                          map = (fmap toBitBuilder . toHashMap) tree
                          treebits = encodeTree tree
                      in HuffTree map treebits
    where encodeTree :: Tree a -> StructureBits
          encodeTree (Leaf _ _) = [False]
          encodeTree (Node l r) = encodeTree l ++ encodeTree r

          toHashMap :: Tree (a, String) -> M.HashMap a BitBuilder
          toHashMap tree = go tree M.empty
            where go (Leaf (a, bits) _) map = M.insert a bits map
                  go (Node l r) map = go l map `M.union` go r map

          toBitBuilder :: String -> B.BitBuilder
          toBitBuilder = foldl (\acc char -> B.append acc (c2b char)) B.empty
          c2b '0' = B.singleton False
          c2b '1' = B.singleton True

-- | Serializes the Tree into a ByteString
serializeTree :: (Serializable a) => HuffTree a -> S.ByteString
serializeTree (HuffTree map treebits) = serialize (M.size map)
                              `mappend` serialize (M.keys map)
                              `mappend` serialize treebytes

-- | Combines all 'Tree's according to Huffmann into one single 'Tree'
combineAll :: [Tree a] -> Tree a
combineAll ts
    | length ts > 1  = combineAll (combineSmallest ts)
    | length ts == 1 = ts !! 0
    | otherwise      = error "node without kids should be a leaf"

-- | Combines the two nodes with the smallest weight on this level.
combineSmallest :: [Tree a] -> [Tree a]
combineSmallest [] = []
combineSmallest (a:[]) = a : []
combineSmallest (a:b:[]) = [Node a b]
combineSmallest (a:b:xs)
    | weight a < weight b = combineSmallest' (a:b:xs)
    | otherwise           = combineSmallest' (b:a:xs)
  where combineSmallest' (a:b:c:xs)
            | cw < bw   = b : combineSmallest (if cw < aw then c:a:xs else a:c:xs)
            | otherwise = c : combineSmallest (a:b:xs)
                where (aw, bw, cw) = (weight a, weight b, weight c)

-- | Assignes the bits to each node according to Huffmann
assignCodes :: Tree a -> Tree (a, String)
assignCodes root = go root []
    where go (Leaf a w) code = Leaf (a, code) w
          go (Node l r) code = Node (go l (code++"1"))
                                    (go r (code++"0"))
