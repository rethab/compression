{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Compression.Huffman.Encoder where

import Data.Hashable
import Data.Monoid         (mappend)
import Data.Word           (Word8)

import qualified Data.Binary.BitBuilder    as B
import qualified Data.Binary.Put           as BinPut
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
type StructureBits = [Bool]

-- the list of values is actually a dupicate of the hashmap. it is required
-- since order of the values is crucial in order to serialize it and the
-- hashmap discards the order
data HuffTree a = HuffTree (M.HashMap a B.BitBuilder) [a] StructureBits
    deriving (Show)

class Serializable a where
    serialize :: a -> S.ByteString

instance Serializable StructureBits where
    serialize = toBS . foldl mappend B.empty . map B.singleton
      where toBS = L.toStrict . B.toLazyByteString

instance Serializable Int where
     serialize  = L.toStrict . BinPut.runPut . BinPut.putWord64be . fromIntegral

instance Serializable Word8 where
    serialize = L.toStrict . BinPut.runPut . BinPut.putWord8

-- | Searches a the bits for a value
lookupBits :: (Eq a, Hashable a) => HuffTree a -> a -> Maybe B.BitBuilder
lookupBits (HuffTree hmap _ _) needle = M.lookup needle hmap

-- | Creates a Huffmann Tree from a List of Values and their probablities
createHuffTree :: (Eq a, Hashable a) => [(a, Float)] -> HuffTree a
createHuffTree vals = let tree = (assignCodes . combineAll . map Leaf) vals
                          hmap = (fmap toBitBuilder . toHashMap) tree
                          treebits = encodeTree tree
                          orderedvals = leavesvals tree
                      in HuffTree hmap orderedvals treebits

leavesvals :: Tree (a, b) -> [a]
leavesvals (Leaf (v, _)) = [v]
leavesvals (Node l r) = leavesvals l ++ leavesvals r

encodeTree :: Tree (a, String) -> StructureBits
encodeTree (Leaf _) = [False]
encodeTree (Node l r) = True : (encodeTree l ++ encodeTree r)

toHashMap :: (Eq a, Hashable a) => Tree (a, String) -> M.HashMap a String
toHashMap tree = go tree M.empty
    where go (Leaf (a, bits)) map = M.insert a bits map
          go (Node l r) map = go l map `M.union` go r map


-- | Encodes a string of bits into a builder
toBitBuilder :: String -> B.BitBuilder
toBitBuilder = foldl (\acc char -> B.append acc (c2b char)) B.empty
    where c2b :: Char -> B.BitBuilder
          c2b '0' = B.singleton False
          c2b '1' = B.singleton True

-- | Serializes the Tree into a ByteString
serializeTree :: (Serializable a) => HuffTree a -> Word8 -> S.ByteString
serializeTree (HuffTree _ vals treebits) offset = serialize treebits 
                                        `mappend` serialize offset
                                        `mappend` S.concat (map serialize vals)

-- | Combines all 'Tree's according to Huffmann into one single 'Tree'
combineAll :: [Tree (a, Float)] -> Tree (a, Float)
combineAll []     = error "empty list cannot be combined"
combineAll (t:[]) = t
combineAll ts     = combineAll (combineSmallest ts)

-- | Combines the two nodes with the smallest weight on this level.
combineSmallest :: [Tree (a, Float)] -> [Tree (a, Float)]
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
assignCodes :: Tree (a, Float) -> Tree (a, String)
assignCodes (Leaf (a, _)) = Leaf (a, "1")
assignCodes root = go root []
    where go (Leaf (a, w)) code = Leaf (a, code)
          go (Node l r) code    = Node (go l (code++"1"))
                                       (go r (code++"0"))

weight :: Tree (a, Float) -> Float
weight (Leaf (_, w)) = w
weight (Node r l)    = go 0 r l
    where go acc (Leaf (_, w1)) (Leaf (_, w2)) = acc + w1 + w2
          go acc (Leaf (_, w)) (Node l1 r1)   = go (acc + w) l1 r1
          go acc (Node l1 r1) (Leaf (_, w))   = go (acc + w) l1 r1
          go acc (Node r1 l1) (Node r2 l2) = go (go acc r1 l1) r2 l2
