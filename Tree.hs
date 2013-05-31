module Tree where

import Data.HashMap.Strict as M

data Tree a = Leaf a Float
            | Node (Tree a) (Tree a)
            deriving (Show, Eq)

isLeaf :: Tree a -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

weight :: Tree a -> Float
weight (Leaf _ w) = w
weight (Node r l) = go 0 r l
    where go acc (Leaf _ w1) (Leaf _ w2)   = acc + w1 + w2
          go acc (Leaf _ w) (Node l1 r1)   = go (acc + w) l1 r1
          go acc (Node l1 r1) (Leaf _ w)   = go (acc + w) l1 r1
          go acc (Node r1 l1) (Node r2 l2) = go (go acc r1 l1) r2 l2

-- | Combines the all 'Tree's according to Huffmann into one single 'Tree'
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
