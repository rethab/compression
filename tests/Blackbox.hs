{-# LANGUAGE TemplateHaskell #-}

module Blackbox where

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All

import Compression.Huffman

import qualified Data.ByteString        as BS

main = $quickCheckAll

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

prop_reverse_function bs = not (BS.null bs) && BS.length bs `mod` 4 == 0  ==>
    (decode . encode) bs == bs

