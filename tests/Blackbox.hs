{-# LANGUAGE TemplateHaskell #-}

module Blackbox where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All

import Compression.Huffman
import Compression.LZW

import qualified Data.ByteString        as BS

main = $quickCheckAll

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

prop_reverse_function_huff bs = not (BS.null bs) && BS.length bs `mod` 4 == 0  ==>
    (decode . encode) bs == bs

prop_reverse_function_lzw bs = not (BS.null bs) && BS.length bs `mod` 4 == 0  ==>
    (decompress . compress) bs == bs
