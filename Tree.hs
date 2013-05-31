module Tree where

import Control.Arrow       (second)
import Control.Monad       (mplus)

import qualified Data.Binary.BitBuilder as B

data Tree a = Leaf a Float
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

instance Functor Tree where
    fmap f (Leaf v w) = Leaf (f v) w
    fmap f (Node r l) = Node (fmap f r) (fmap f l)

-- | Creates a Huffmann Tree from a List of Values and their probablities
toHuffTree :: [(a, Float)] -> Tree (a, B.BitBuilder)
toHuffTree = fmap (second toBitBuilder) . assignCodes . combineAll . map toLeaf
    where toLeaf (a, p) = Leaf a p

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
          go (Node r l) code = Node (go r (code++"1"))
                                    (go l (code++"0"))
