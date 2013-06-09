{-# LANGUAGE TemplateHaskell #-}

module Encoder where

import Control.Monad       (mplus, liftM, liftM2)
import Data.Bits           (testBit)
import Data.List           (isPrefixOf, nubBy)
import Data.Maybe          (isJust)
import Data.Monoid         (mappend)
import Data.Hashable
import Data.Word           (Word32)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.HUnit
import Debug.Trace          (trace)

import Compression.Huffman.Encoder

import qualified Data.ByteString        as S
import qualified Data.ByteString.Lazy   as L
import qualified Data.Binary.BitBuilder as B
import qualified Data.Binary.Strict.Get as BinGet
import qualified Data.HashMap.Strict    as M

main = runhunit >> $quickCheckAll

-- equivalence function for floating point based on relative error
infix 4 ~=~
(~=~) :: (Ord a, Floating a) => a -> a -> Bool
(~=~) 0 b = (abs (b - 0)) < 0.001
(~=~) a b = (abs (b - a)) / a < 0.001

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [leaf, node]
        where leaf = liftM Leaf arbitrary
              node = liftM2 Node arbitrary arbitrary

-- string of bits to test binary encodings
newtype BitString = BitString String deriving (Show)

instance Arbitrary BitString where
    arbitrary = liftM BitString bits
        where bits = suchThat (listOf $ elements ['0', '1']) bytelen
              bytelen [] = False
              bytelen xs = mod (length xs) 8 == 0

-- flat tree for input to huffmann tree construction
newtype Leaves a = Leaves [Tree (a, Float)] deriving (Show)

instance (Arbitrary a) => Arbitrary (Leaves a) where
    arbitrary = liftM Leaves kids
        where kids = listOf leaf 
              leaf :: (Arbitrary a) => Gen (Tree (a, Float))
              leaf = do val <- arbitrary
                        weight <- positiveWeight
                        return $ Leaf (val, weight)
              positiveWeight = suchThat arbitrary (\i -> i > 0)

instance (Arbitrary a, Hashable a, Eq a) => Arbitrary (HuffTree a) where
    arbitrary = do hmap <- liftM M.fromList arbitrary
                   bits <- vector (M.size hmap) 
                   return $ HuffTree hmap (M.keys hmap) bits

instance Arbitrary B.BitBuilder where
    arbitrary = return B.empty

listweight = foldl (\acc k -> weight k + acc) 0

height :: Tree a -> Int
height (Leaf _)  = 1
height (Node r l) = height r + height l

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree a -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

isLeaf :: Tree a -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

read64 :: S.ByteString -> Int
read64 bs = let (res, _) = BinGet.runGet BinGet.getWord64be bs
            in case res of
                 Left e -> error e
                 Right v -> fromIntegral v

lookupCode :: (Eq a) => Tree (a, String) -> a -> Maybe String
lookupCode (Leaf (a, code)) needle | a == needle = Just code
                                   | otherwise = Nothing
lookupCode (Node l r) needle = lookupCode l needle `mplus` lookupCode r needle

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

prop_prefix_free tree = height tree < 40 ==>
    let codes = collectCodes (assignCodes tree)
        collectCodes (Leaf (_, code)) = [code]
        collectCodes (Node l r) = collectCodes l ++ collectCodes r
    in not (any (\code -> any (\other -> code /= other && code `isPrefixOf` other) codes) codes)

prop_none_empty tree =
    let code = assignCodes tree
        nonemptycode (Leaf (_, code)) = not $ null code
        nonemptycode (Node l r) = nonemptycode l || nonemptycode r
    in nonemptycode code

prop_permutation (Leaves tree) =
    let codes = assignCodes (combineAll tree)
    in all (\(Leaf (c, _)) -> isJust (lookupCode codes c)) tree

prop_same_bits (BitString bs) = 
    let bytes = B.toLazyByteString (toBitBuilder bs)
        biteq byte string = all id $ map (\i -> testBit byte (7-i) == (string !! i == '1')) [0..7]
        splitlen :: Int -> String -> [String]
        splitlen len xs =
            if len > length xs then [xs]
            else let (y,ys) = splitAt len xs in y : (splitlen len ys)
    in all id $ zipWith biteq (L.unpack bytes) (splitlen 8 bs)
    
prop_perm_hufftree leaves = not (null leaves) ==>
    let hufftree = createHuffTree leaves
    in all (\(v, _) -> isJust $ lookupBits hufftree v) leaves

prop_leaves_equals_zeros_in_bits leaves = not (null leaves) ==>
    let (HuffTree _ _ bits) = createHuffTree leaves
    in length (filter not bits) == length leaves

prop_bytes_equal_sum :: HuffTree Word32 -> Property
prop_bytes_equal_sum tree@(HuffTree hmap vals bits) = length bits > 3 ==>
    let bs = serializeTree tree 0
        structurebytes = ceiling ((fromIntegral (length bits)) / 8) -- ceiling to next byte
        expected = structurebytes + 1 + (length vals) * 4
    in S.length bs == expected

prop_leaves_equal_false tree = 
    let bits = encodeTree tree
    in leaves tree == length (filter not bits)

prop_nodes_equal_true tree =
    let bits = encodeTree tree
    in nodes tree == length (filter id bits)

prop_last_is_false tree = not (isLeaf tree) ==>
    let bits = encodeTree tree
    in last bits == False

prop_one_more_leaf_than_nodes tree = height tree < 100 ==>
    let bits = encodeTree tree
    in length (filter id bits) + 1 == length (filter not bits)

prop_functor_law_identity :: (Eq a) => Tree a -> Bool
prop_functor_law_identity tree = fmap id tree == tree

prop_functor_law_composability :: (Eq a, Show a) => Tree a -> Bool
prop_functor_law_composability tree = fmap (length . show) tree == (fmap length . fmap show) tree

prop_same_bits_as_in_string values = not (null values) && nubBy (\(a,_) (b,_)-> a == b) values == values ==>
    let hufftree = createHuffTree values
        stringtree = (assignCodes . combineAll . map Leaf) values
    in compareWithHuffBits stringtree hufftree
  where compareWithHuffBits (Node l r) tree = compareWithHuffBits l tree && compareWithHuffBits r tree
        compareWithHuffBits (Leaf (val, code)) tree = bitsequal code (lookupBits tree val)
        biteq byte string = all id $ map (\i -> testBit byte (7-i) == (bit i == '1')) [0..7]
            where bit i = if i >= length string then '0' else string !! i
        splitlen :: Int -> String -> [String]
        splitlen len xs =
            if len > length xs then [xs]
            else let (y,ys) = splitAt len xs in y : (splitlen len ys)
        bitsequal :: String -> Maybe B.BitBuilder -> Bool
        bitsequal _ Nothing = False
        bitsequal bits (Just builder) = let words = L.unpack $ B.toLazyByteString builder
                                        in all id $ zipWith biteq words (splitlen 8 bits)

-- HUnit Tests

runhunit = mapM_ runTestTT $  map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("star_sample", TestCase star_sample)
                   , ("create_huff_tree", TestCase create_huff_tree)
                   , ("create_huff_tree2", TestCase create_huff_tree2)
                   , ("create_huff_tree3", TestCase create_huff_tree3)
                   , ("serialize_tree", TestCase serialize_tree)
                   , ("serialize_tree2", TestCase serialize_tree2)
                   , ("serialize_tree3", TestCase serialize_tree3)
                   , ("serialize_tree4", TestCase serialize_tree4)
                   , ("test_leavesvals", TestCase test_leavesvals)
                   , ("test_leavesvals2", TestCase test_leavesvals2)
                   ]
star_sample = let nodes = [ Leaf ('*', 0.65), Leaf ('+', 0.15), Leaf ('#', 0.1), Leaf ('?', 0.04)
                          , Leaf ('~', 0.02), Leaf ('$', 0.04)]
                  codes = assignCodes (combineAll nodes)
              -- while huffmann always creates an optimal tree, 
              -- there may be more than one that is optimal
              in do lookupCode codes '*' @?= Just "1"
                    lookupCode codes '+' @?= Just "01"
                    lookupCode codes '#' @?= Just "000"
                    lookupCode codes '$' @?= Just "0011"
                    lookupCode codes '?' @?= Just "00100"
                    lookupCode codes '~' @?= Just "00101"

create_huff_tree = let tree = createHuffTree [(123 :: Word32, 1.0)]
                       (Just builder) = lookupBits tree 123
                       bytes = L.toStrict $ B.toLazyByteString builder
                       -- remember that we are filling bits from the left side ;)
                   in do S.unpack bytes @?= [128]

-- should reorder values in list of values and put the most one most likely
-- to occur first
create_huff_tree2 = let probs =  [ (252644608, 0.25)
                                 , (84020489, 0.5)
                                 , (101583631, 0.25) ] :: [(Word32, Float)]
                        (HuffTree _ vals bits) = createHuffTree probs
                    in do head vals @?= 84020489
                          bits @?= [True, False, True, False, False]

create_huff_tree3 = let probs =  [ (252644608, 0.5)
                                 , (84020489, 0.25)
                                 , (101583631, 0.25) ] :: [(Word32, Float)]
                        (HuffTree hmap _ _) = createHuffTree probs
                        wrds = S.unpack . L.toStrict . B.toLazyByteString
                        Just b1 = M.lookup 252644608 hmap
                        Just b2 = M.lookup 84020489 hmap
                        Just b3 = M.lookup 101583631 hmap
                    in do wrds b1 @?= [128]
                          wrds b2 @?= [64]
                          wrds b3 @?= [0]

serialize_tree = let htree = HuffTree (M.insert (123 :: Word32) undefined M.empty)
                                      [123] [False]
                     serialized = S.unpack $ serializeTree htree 3
                     tree = [0]
                     off = [3]
                     vals = [0, 0, 0, 123]
                     expected = tree ++ off ++ vals
                 in do serialized @?= expected 

serialize_tree2 = let htree = HuffTree (M.insert (131328 :: Word32) undefined M.empty)
                                       [131328] [False]
                      serialized = S.unpack $ serializeTree htree 7
                      tree = [0]
                      off = [7]
                      vals = [0, 2, 1, 0]
                      expected = tree ++ off ++ vals
                  in do serialized @?= expected 

serialize_tree3 = let htree = HuffTree (M.insert (117571840 :: Word32) undefined M.empty)
                                       [117571840] [False]
                      serialized = S.unpack $ serializeTree htree 0
                      tree = [0]
                      off = [0]
                      vals = [7, 2, 1, 0]
                      expected = tree ++ off ++ vals
                  in do serialized @?= expected 

serialize_tree4 = let htree = HuffTree (M.fromList [(101188357, undefined), (134350087, undefined)])
                                       [101188357, 134350087] [True, False, False] :: HuffTree Word32
                      serialized = S.unpack $ serializeTree htree 2
                      tree = [128]
                      off = [2]
                      vals = [6, 8, 3, 5, 8, 2, 5, 7]
                      expected = tree ++ off ++ vals
                  in do serialized @?= expected 

test_leavesvals = let tree = Node (Node (Leaf (1, undefined))
                                        (Leaf (2, undefined)))
                                  (Leaf (3, undefined))
                      vals = leavesvals tree
                  in vals @?= [1, 2, 3]

test_leavesvals2 = let tree = Node (Leaf (2, undefined)) (Leaf (3, undefined))
                       vals = leavesvals tree
                   in vals @?= [2, 3]
