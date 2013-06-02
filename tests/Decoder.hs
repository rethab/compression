{-# LANGUAGE TemplateHaskell #-}

module Decoder where

import Control.Monad       (liftM, liftM2)
import Data.Bits           (testBit)
import Data.List           (isPrefixOf)
import Data.Maybe          (isJust)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.HUnit

import Compression.Huffman.Decoder

import qualified Data.ByteString.Lazy   as BS
import qualified Data.Binary.BitBuilder as B

main = runhunit >> $quickCheckAll

-- equivalence function for floating point based on relative error
infix 4 ~=~
(~=~) :: (Ord a, Floating a) => a -> a -> Bool
(~=~) 0 b = (abs (b - 0)) < 0.001
(~=~) a b = (abs (b - a)) / a < 0.001

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [leaf, node]
        where leaf = liftM2 Leaf arbitrary arbitrary
              node = liftM2 Node arbitrary arbitrary

-- string of bits to test binary encodings
newtype BitString = BitString String deriving (Show)

instance Arbitrary BitString where
    arbitrary = liftM BitString bits
        where bits = suchThat (listOf $ elements ['0', '1']) bytelen
              bytelen [] = False
              bytelen xs = mod (length xs) 8 == 0

-- flat tree for input to huffmann tree construction
newtype Leaves a = Leaves [Tree a] deriving (Show)

instance (Arbitrary a) => Arbitrary (Leaves a) where
    arbitrary = liftM Leaves kids
        where kids = listOf $ liftM2 Leaf arbitrary positiveWeight
              positiveWeight = suchThat arbitrary (\i -> i > 0)

listweight = foldl (\acc k -> weight k + acc) 0

height :: Tree a -> Int
height (Leaf _ _) = 1
height (Node r l) = height r + height l

isLeaf :: Tree a -> Bool
isLeaf (Leaf _ _) = True
isLeaf _ = False

prop_preseve_weight_once (Leaves kids) =
    listweight (combineSmallest kids) ~=~ listweight kids

prop_preserve_weight_total (Leaves kids) = not (null kids) ==> 
    weight (combineAll kids) ~=~ listweight kids

prop_one_node_less (Leaves kids) = length kids > 1 ==>
    length (combineSmallest kids) == (length kids - 1)

prop_combined_leaves_are_smallest (Leaves kids) = length kids > 1 ==>
    let combined = combineSmallest kids
        Node r l = head $ filter (not . isLeaf) combined
        -- bigger leaf of the two is still smaller than all others
        bigger = if weight r > weight l then weight r else weight l
    in all (\l -> weight l >= bigger) combined

prop_prefix_free tree = height tree < 50 ==>
    let codes = collectCodes (assignCodes tree)
        collectCodes (Leaf (_, code) _) = [code]
        collectCodes (Node l r) = collectCodes l ++ collectCodes r
    in not (any (\code -> any (\other -> code /= other && code `isPrefixOf` other) codes) codes)

prop_permutation (Leaves tree) =
    let codes = assignCodes (combineAll tree)
    in all (\(Leaf c _) -> isJust (lookupCode codes c)) tree

prop_same_bits (BitString bs) = 
    let bytes = B.toLazyByteString (toBitBuilder bs)
        biteq byte string = all id $ map (\i -> testBit byte (7-i) == (string !! i == '1')) [0..7]
        splitlen :: Int -> String -> [String]
        splitlen len xs =
            if len > length xs then [xs]
            else let (y,ys) = splitAt len xs in y : (splitlen len ys)
    in all id $ zipWith biteq (BS.unpack bytes) (splitlen 8 bs)

prop_ser_compose tree = height tree > 50 ==>
    let dec = (deserialize . serialize) tree
    in all id 


-- HUnit Tests

runhunit = mapM_ runTestTT $  map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("star_sample", TestCase star_sample)
                   ]
star_sample = let nodes = [ Leaf '*' 0.65, Leaf '+' 0.15, Leaf '#' 0.1, Leaf '?' 0.04
                          , Leaf '~' 0.02, Leaf '$' 0.04]
                  codes = assignCodes (combineAll nodes)
              -- while huffmann always creates an optimal tree, 
              -- there may be more than one that is optimal
              in do lookupCode codes '*' @?= Just "1"
                    lookupCode codes '+' @?= Just "01"
                    lookupCode codes '#' @?= Just "000"
                    lookupCode codes '$' @?= Just "0011"
                    lookupCode codes '?' @?= Just "00100"
                    lookupCode codes '~' @?= Just "00101"
