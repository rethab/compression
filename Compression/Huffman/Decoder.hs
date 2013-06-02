module Compression.Huffman.Decoder where

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

data Tree a = Leaf a Float
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf v w) = Leaf (f v) w
    fmap f (Node r l) = Node (fmap f r) (fmap f l)

-- | Creates a Huffmann Tree from a List of Values and their probablities
toHuffTree :: (Eq a, Hashable a) => [(a, Float)] -> M.HashMap a B.BitBuilder
toHuffTree = fmap toBitBuilder . toStringHuffTree

toStringHuffTree :: (Eq a, Hashable a) => [(a, Float)] -> M.HashMap a String
toStringHuffTree = toHashMap . assignCodes . combineAll . map toLeaf
    where toLeaf (a, p) = Leaf a p
          toHashMap tree = go tree M.empty
          go (Leaf (a, bits) _) map = M.insert a bits map
          go (Node l r) map = go l map `M.union` go r map

-- | Encodes a string into a sequence of Bits
toBitBuilder :: String -> B.BitBuilder
toBitBuilder = foldl (\acc char -> B.append acc (c2b char)) B.empty
    where c2b '0' = B.singleton False
          c2b '1' = B.singleton True

weight :: Tree a -> Float
weight (Leaf _ w) = w
weight (Node r l) = go 0 r l
    where go acc (Leaf _ w1) (Leaf _ w2)   = acc + w1 + w2
          go acc (Leaf _ w) (Node l1 r1)   = go (acc + w) l1 r1
          go acc (Node l1 r1) (Leaf _ w)   = go (acc + w) l1 r1
          go acc (Node r1 l1) (Node r2 l2) = go (go acc r1 l1) r2 l2

-- | Searches a Code for a specific entry in the tree
lookupBits :: (Eq a) => Tree (a, B.BitBuilder) -> a -> Maybe B.BitBuilder
lookupBits (Leaf (a, code) _) needle = if a == needle then Just code else Nothing
lookupBits (Node l r) needle = lookupBits l needle `mplus` lookupBits r needle

-- | Searches a Code for a specific entry in the tree
lookupCode :: (Eq a) => Tree (a, String) -> a -> Maybe String
lookupCode (Leaf (a, code) _) needle = if a == needle then Just code else Nothing
lookupCode (Node l r) needle = lookupCode l needle `mplus` lookupCode r needle

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

assignCodes :: Tree a -> Tree (a, String)
assignCodes root = go root []
    where go (Leaf a w) code = Leaf (a, code) w
          go (Node l r) code = Node (go l (code++"1"))
                                    (go r (code++"0"))

serialize :: Tree (Word32, B.BitBuilder) -> S.ByteString
serialize t = let (nleaves, values) = (first toBS) (serleaves t)
                  treebytes = (L.toStrict . B.toLazyByteString) (sertree t)
              in nleaves `mappend` values `mappend` treebytes
    where serleaves (Leaf (val, _) _ ) = (1, toBS val)
          serleaves (Node l r) = let (nr, left)  = serleaves l
                                     (nl, right) = serleaves r
                                 in  (nr+nl, left `mappend` right)
          toBS = L.toStrict . BinPut.runPut . BinPut.putWord32be 
          sertree (Leaf _ _) = B.singleton False
          sertree (Node l r) = B.singleton True `mappend` sertree l
                     `mappend` B.singleton True `mappend` sertree r

deserialize :: S.ByteString -> (M.HashMap String Word32, S.ByteString)
deserialize bs = let (nleaves, bs') = (first (fromIntegral . head)) $ read32 bs 1
                     (leaves, bs'') = read32 bs' nleaves
                     decoder = readDecoder bs'' leaves
                     -- assumption: a binary tree (not necessarily balanced)
                     -- with n leaves has (n-1) nodes. thus the number of bits
                     -- read is the sum of the two
                     nbytes = ceiling $ (fromIntegral (nleaves + (nleaves - 1))) / 8
                 in (decoder, S.drop nbytes bs'')

read32 :: S.ByteString -> Int -> ([Word32], S.ByteString)
read32 bs 0 = ([], bs)
read32 bs n = let (res, bs') = BinGet.runGet BinGet.getWord32be bs
              in case res of
                   Left e -> error e
                   Right val -> first (val:) (read32 bs' (n-1))

readDecoder :: S.ByteString -> [Word32] -> M.HashMap String Word32
readDecoder bs leaves = case BitGet.runBitGet bs readTree of
                          Left e -> error e
                          Right tree -> mkDecoder tree leaves

mkDecoder :: BTree -> [Word32] -> M.HashMap String Word32
mkDecoder root values = snd $ go root values [] M.empty
    where go Leaf_ vals code map = (1, M.insert code (head vals) map)
          go (Node_ l r) vals code map =
            let (nleaves, map') = go l vals (code++"1") map
            in go r (drop nleaves vals) (code++"0") map'

data BTree = Node_ BTree BTree | Leaf_ deriving (Show)

readTree :: BitGet.BitGet BTree
readTree = BitGet.getBit >>= \set ->
    if set then return Leaf_ else liftM2 Node_ readTree readTree
