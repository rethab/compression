{-# LANGUAGE TemplateHaskell #-}

module Huffman where

import Control.Monad (liftM, liftM2)
import Data.Bits
import Data.Word
import Data.List     ((\\))
import Data.Monoid   (mappend)

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All
import Test.HUnit
import Debug.Trace (trace)

import Compression.Huffman
import qualified Compression.Huffman.Encoder as E

import qualified Data.Binary.Strict.Get as BinStrict
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as M
import qualified Data.Binary.BitBuilder as B


import qualified Encoder as Encoder
import qualified Decoder as Decoder

main = Encoder.main >> Decoder.main >> mapM_ runTestTT units >> $quickCheckAll

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

prop_first_four bs = BS.length bs >= 4 ==>
    let ((Right out), _) = BinStrict.runGet parse32 bs
        (Just (exp, _))  = readWord32 bs
    in out == exp

prop_consume_bytes bs = BS.length bs >= 4 ==>
    let ((Right _), bs') = BinStrict.runGet parse32 bs
    in bs' /= bs

prop_prob_sum_is_one words = not (null words) ==>
    let ps = probs words
        sm = foldl (+) 0 (map snd ps)
    in (abs (sm - 1)) < 0.001

prop_first_ident bs = BS.length bs >= 4 ==>
    let out = parse bs !! 0
        ((Right exp), _) = BinStrict.runGet parse32 bs
    in out == exp

grop_len_mod4 bs = 
    let out = parse bs
        len = BS.length bs
    in fromIntegral (length out) == (fromIntegral (len - (len `mod` 4))) / 4

-- must be mod 4 because otherwise the tail is truncated thus cannot be compared. only for 32 bit though
prop_reverse_function bs = not (BS.null bs) && BS.length bs `mod` 4 == 0  ==>
    let bs' = trace (show $ BS.unpack bs) bs
    in (decode . encode) bs' == bs'

-- HUnit
units = map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("props_one_entry", TestCase probs_one_entry)
                   , ("probs_two_entries", TestCase probs_two_entries)
                   , ("probs_two_entries2", TestCase probs_two_entries2)
                   , ("probs_three_entries", TestCase probs_three_entries)
                   , ("probs_three_entries2", TestCase probs_three_entries2)
                   , ("probs_three_entries3", TestCase probs_three_entries3)
                   , ("probs_four_entries", TestCase probs_four_entries)
                   , ("probs_four_entries1", TestCase probs_four_entries)
                   , ("probs_four_entries2", TestCase probs_four_entries2)
                   , ("probs_four_entries3", TestCase probs_four_entries3)
                   , ("probs_four_entries4", TestCase probs_four_entries4)
                   , ("probs_four_entries5", TestCase probs_four_entries5)
                   , ("probs_four_entries6", TestCase probs_four_entries6)
                   , ("hufftree_ser_roundtrip", TestCase hufftree_ser_roundtrip)
                   , ("decode_huff", TestCase decode_huff)
                   , ("decode_huff2", TestCase decode_huff2)
                   , ("testdecode", TestCase testdecode)
                   , ("testencodevalues", TestCase testencodevalues)
                   , ("count_remaing", TestCase count_remaining)
                   ]

-- HUnit: Probabilities
probs_one_entry = let ps = probs [1]
                      Just val = lookup 1 ps
                  in do val @?= 1

probs_two_entries = let ps = probs [1, 2]
                        Just a = lookup 1 ps
                        Just b = lookup 2 ps
                    in do a @?= 0.5
                          b @?= 0.5

probs_two_entries2 = let ps = probs [1, 1]
                         Just a = lookup 1 ps
                     in do a @?= 1

probs_three_entries = let ps = probs [1, 2, 3]
                          Just a = lookup 1 ps
                          Just b = lookup 2 ps
                          Just c = lookup 3 ps
                      in do a @?= (1/3)
                            b @?= (1/3)
                            c @?= (1/3)

probs_three_entries2 = let ps = probs [1, 2, 1]
                           Just a = lookup 1 ps
                           Just b = lookup 2 ps
                       in do a @?= (2/3)
                             b @?= (1/3)

probs_three_entries3 = let ps = probs [1, 1, 1]
                           Just a = lookup 1 ps
                       in do a @?= 1

probs_four_entries = let ps = probs [1, 1, 1, 1]
                         Just a = lookup 1 ps
                     in do a @?= 1

probs_four_entries2 = let ps = probs [1, 1, 1, 2]
                          Just a = lookup 1 ps
                          Just b = lookup 2 ps
                      in do a @?= 0.75
                            b @?= 0.25

probs_four_entries3 = let ps = probs [1, 1, 2, 2]
                          Just a = lookup 1 ps
                          Just b = lookup 2 ps
                      in do a @?= 0.5
                            b @?= 0.5

probs_four_entries4 = let ps = probs [1, 2, 2, 2]
                          Just a = lookup 1 ps 
                          Just b = lookup 2 ps 
                      in do a @?= 0.25
                            b @?= 0.75

probs_four_entries5 = let ps = probs [1, 2, 3, 2]
                          Just a = lookup 1 ps
                          Just b = lookup 2 ps
                          Just c = lookup 3 ps
                      in do a @?= 0.25
                            b @?= 0.5
                            c @?= 0.25

probs_four_entries6 = let ps = probs [1, 4, 3, 2]
                          Just a = lookup 1 ps 
                          Just b = lookup 2 ps
                          Just c = lookup 3 ps
                          Just d = lookup 4 ps
                      in do a @?= 0.25
                            b @?= 0.25
                            c @?= 0.25
                            d @?= 0.25

testencodevalues = let hmap = M.fromList [(257, B.singleton False)]
                       vals = [257]
                       bits = [False]
                       hufftree = E.HuffTree hmap vals bits
                       rawbytes = BS.pack [0, 0, 1, 1]
                       (off, bytes) = encodevals hufftree rawbytes
                   in do BS.unpack bytes @?= [0]
                         off @?= 7

hufftree_ser_roundtrip = let tree = E.HuffTree (M.fromList [(3 :: Word32, undefined)
                                                          , (4 :: Word32, undefined)
                                                          , (5 :: Word32, undefined)
                                                          , (6 :: Word32, undefined)])
                                               [3, 4, 5, 6]
                                               [True, True, False, False, True, False, False]
                             bs = E.serializeTree tree 3
                             rest = BS.pack [1, 2, 3]
                             (huff, off, rest') = readHuff (bs `mappend` rest)
                         in do M.lookup "11" huff @?= Just 3
                               M.lookup "10" huff @?= Just 4
                               M.lookup "01" huff @?= Just 5
                               M.lookup "00" huff @?= Just 6
                               rest @?= rest'
                               off @?= 3

decode_huff = let hmap = M.fromList [("1", 42), ("01", 137),  ("001", 111)]
                  bytes = BS.pack [164] 
                  expected = BS.pack [0, 0, 0, 42, 0, 0, 0, 137, 0, 0, 0, 111]
              in do decodeHuff hmap bytes 0 @?= expected

decode_huff2 = let hmap = M.fromList [("1",1677824), ("0",84214021)]
                   bytes = BS.pack [64] 
                   expected = [5,5,1,5,1,0,4,8]
               in do BS.unpack (decodeHuff hmap bytes 0) @?= expected

testdecode = let nvals = BS.pack [0, 0, 0, 0, 0, 0, 0, 3]
                 vals = BS.pack [0, 0, 0, 42, 0, 0, 0, 137, 0, 0, 0, 111]
                 bits = BS.pack [160]
                 encoded = BS.pack [165, 160]
                 combined = nvals `mappend` vals `mappend` bits `mappend` encoded
             in do BS.unpack (decode combined) @?= [0, 0, 0, 42, 0, 0, 0, 137
                                              , 0, 0, 0, 111, 0, 0, 0, 42
                                              , 0, 0, 0, 137, 0, 0, 0, 42
                                              , 0, 0, 0, 137, 0, 0, 0, 111
                                              , 0, 0, 0, 111]

count_remaining = let one = B.singleton False
                      two = one `mappend` one
                      three = two `mappend` one
                      four = three `mappend` one
                      five = four `mappend` one
                      six = five `mappend` one
                      seven = six `mappend` one
                      eight = seven `mappend` one
                      nine = eight `mappend` one
                  in do countRemaining one @?= 7
                        countRemaining two @?= 6
                        countRemaining three @?= 5
                        countRemaining four @?= 4
                        countRemaining five @?= 3
                        countRemaining six @?= 2
                        countRemaining seven @?= 1
                        countRemaining eight @?= 0
                        countRemaining nine @?= 7

-- Utilities

-- | Reads four 'Word8's and puts them together into a 'Word32'
readWord32 :: BS.ByteString -> Maybe (Word32, BS.ByteString)
readWord32 bs = do (a, bs')    <- BS.uncons bs
                   (b, bs'')   <- BS.uncons bs'
                   (c, bs''')  <- BS.uncons bs''
                   (d, bs'''') <- BS.uncons bs'''
                   Just (to32 a b c d, bs'''')

-- | Puts 4 'Word8's together to a 'Word32'

to32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
to32 a b c d = shiftL (fromIntegral a) 24
           .|. shiftL (fromIntegral b) 16
           .|. shiftL (fromIntegral c)  8
           .|. (fromIntegral d)
