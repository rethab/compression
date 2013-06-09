{-# LANGUAGE TemplateHaskell #-}

module Decoder where

import Test.QuickCheck
import Test.QuickCheck.All
import Test.HUnit
import Data.List     ((\\), nubBy)
import Data.Word     (Word8, Word32)
import Control.Monad (liftM2)
import Data.Monoid   (mappend)

import qualified Data.HashMap.Strict       as M
import qualified Data.ByteString           as S
import qualified Data.Binary.Strict.BitGet as BitGet

import Compression.Huffman.Decoder


main = runhunit >> $quickCheckAll

data MkDecoderIn = MkDecoderIn BTree [Word8] deriving (Show)

instance Arbitrary MkDecoderIn where
    arbitrary = do btree <- suchThat arbitrary (\tree -> leaves tree > 0 && leaves tree < 100)
                   words <- suchThat (vector (leaves btree)) unique
                   return $ MkDecoderIn btree words 
                     where unique vals = nubBy (==) vals == vals

instance Arbitrary BTree where
    arbitrary = oneof [return Leaf, liftM2 Node arbitrary arbitrary]

leaves :: BTree -> Int
leaves Leaf = 1
leaves (Node l r) = leaves l + leaves r

nodes :: BTree -> Int
nodes Leaf = 0
nodes (Node l r) = 1 + (nodes l) + (nodes r)

runhunit = mapM_ runTestTT $  map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("blackbox_deserialize", TestCase blackbox_deserialize) 
                   , ("blackbox_deserialize2", TestCase blackbox_deserialize2) 
                   , ("blackbox_deserialize3", TestCase blackbox_deserialize3) 
                   , ("decode_bintree1", TestCase decode_bintree1)
                   , ("decode_bintree2", TestCase decode_bintree2)
                   , ("decode_bintree3", TestCase decode_bintree3)
                   , ("single_leaf_decoder", TestCase single_leaf_decoder)
                   , ("make_decoder_1", TestCase make_decoder_1)
                   , ("make_decoder_2", TestCase make_decoder_2)
                   , ("make_decoder_3", TestCase make_decoder_3)
                   , ("read_8_test", TestCase read_8_test)
                   , ("read_8_test2", TestCase read_8_test2)
                   , ("decode_tree", TestCase decode_tree)
                   , ("decode_tree2", TestCase decode_tree2)
                   , ("decode_tree3", TestCase decode_tree3)
                   ]

blackbox_deserialize = let structure = S.pack [200]
                           off = S.pack [3]
                           leaves = S.pack [0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6]
                           following = S.pack [42, 125] 
                           (dec, off', rest) = deserialize (structure `mappend`
                                                            off `mappend`
                                                            leaves `mappend` following)
                       in do M.lookup "11" dec @?= Just 3
                             M.lookup "10" dec @?= Just 4
                             M.lookup "01" dec @?= Just 5
                             M.lookup "00" dec @?= Just 6
                             rest @?= following
                             off' @?= 3

blackbox_deserialize2 = let structure = S.pack [200]
                            off = S.pack [0]
                            leaves = S.pack [0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6]
                            following = S.pack [42, 125] 
                            (dec, off', rest) = deserialize (structure `mappend` off
                                                            `mappend` leaves
                                                            `mappend` following)
                        in do M.lookup "11" dec @?= Just 3
                              M.lookup "10" dec @?= Just 4
                              M.lookup "01" dec @?= Just 5
                              M.lookup "00" dec @?= Just 6
                              rest @?= following
                              off' @?= 0

blackbox_deserialize3 = let bits = S.pack [160] 
                            off = S.pack [7]
                            vals = S.pack [0, 0, 0, 42, 0, 0, 0, 137, 0, 0, 0 ,111]
                            combined = bits `mappend` off `mappend` vals
                            (huff, off', rest) = deserialize combined
                        in do M.lookup "1" huff @?= Just 42
                              M.lookup "00" huff @?= Just 111
                              M.lookup "01" huff @?= Just 137
                              off' @?= 7

decode_bintree1 = let bits = S.pack [200]
                      (nbits, tree) = case BitGet.runBitGet bits readTree of
                                        Left e -> error e
                                        Right v -> v
                 in do nodes tree @?= 3
                       leaves tree @?= 4
                       nbits @?= 7

decode_bintree2 = let bits = S.pack [168]
                      (nbits, tree) = case BitGet.runBitGet bits readTree of
                                         Left e -> error e
                                         Right v -> v
                 in do nodes tree @?= 3
                       leaves tree @?= 4
                       nbits @?= 7

decode_bintree3 = let bits = S.pack [202,0]
                      (nbits, tree) = case BitGet.runBitGet bits readTree of
                                       Left e -> error e
                                       Right v -> v
                 in do nodes tree @?= 4
                       leaves tree @?= 5
                       nbits @?= 9

single_leaf_decoder = let treebits = [0]
                          off = [7]
                          vals = [0, 2, 1, 0]
                          combined = S.pack (treebits ++ off ++ vals)
                          (dec, off', rest) = deserialize combined
                      in do M.lookup "1" dec @?= Just 131328
                            S.unpack rest @?= []
                            off' @?= 7

-- should read a tree with one node and two leaves (128=100..)
-- and then return the following bytes in the bytestring
decode_tree = let input = S.pack [128, 64, 32]
                  (tree, bs) = decodeTree input
              in do tree @?= Node Leaf Leaf
                    S.unpack bs @?= [64, 32]

decode_tree2 = let input = S.pack [192]
                   (tree, bs) = decodeTree input
               in do tree @?= Node (Node Leaf Leaf) Leaf
                     S.unpack bs @?= []

decode_tree3 = let input = S.pack [202,0,5]
                   (tree, bs) = decodeTree input
               in do tree @?= Node (Node Leaf Leaf)
                                   (Node Leaf (Node Leaf Leaf))
                     S.unpack bs @?= [5]
                  
read_8_test = let input = S.pack [10]
                  (word, rest) = read8 input
              in do word @?= 10
                    rest @?= S.empty

read_8_test2 = let input = S.pack [255, 254]
                   (word, rest) = read8 input
               in do word @?= 255
                     S.unpack rest @?= [254]

make_decoder_1 = let tree = Node Leaf Leaf
                     vals = S.pack [0, 0, 0, 128, 0, 0, 0, 1]
                     (dec, rest) = makeDecoder tree vals
                 in do M.lookup "1" dec @?= Just 128
                       M.lookup "0" dec @?= Just 1
                       M.size dec @?= 2
                       S.unpack rest @?= []

make_decoder_2 = let tree = Leaf
                     vals = S.pack [0, 0, 0, 0]
                     (dec, rest) = makeDecoder tree vals
                 in do M.lookup "1" dec @?= Just 0
                       M.size dec @?= 1
                       S.unpack rest @?= []

make_decoder_3 = let tree = Node Leaf (Node Leaf Leaf)
                     vals = S.pack [0, 0, 0, 128, 1, 1, 1, 1, 0, 0, 0, 0, 3]
                     (dec, rest) = makeDecoder tree vals
                 in do M.lookup "1" dec @?= Just 128
                       M.lookup "01" dec @?= Just 16843009
                       M.lookup "00" dec @?= Just 0
                       M.size dec @?= 3
                       rest @?= S.pack [3]
