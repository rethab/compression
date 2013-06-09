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
    let [out] = parse bs
        (Just (exp, _))  = readWord32 bs
    in out == exp

prop_prob_sum_is_one words = not (null words) ==>
    let ps = probs words
        sm = foldl (+) 0 (map snd ps)
    in (abs (sm - 1)) < 0.001

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
                   , ("testdecode2", TestCase testdecode2)
                   , ("testencodevalues", TestCase testencodevalues)
                   , ("testencodevalues2", TestCase testencodevalues2)
                   , ("count_remaing", TestCase count_remaining)
                   , ("test_encode", TestCase test_encode)
                   , ("test_encode2", TestCase test_encode2)
                   , ("test_encode3", TestCase test_encode3)
                   , ("test_encode4", TestCase test_encode4)
                   , ("test_decode", TestCase test_encode4)
                   ]

test_decode = let encoded = BS.pack [200,0,195,182,250,4,12,137
                                    ,112,209,19,170,237,21,193
                                    ,108,15,254,45]
                  decoded = BS.unpack $ decode encoded
                  expected = [193,108,15,254, 12,137,112,209
                             ,195,182,250,4, 19,170,237,21]
              in do decoded @?= expected

testdecode2 = let encoded = BS.pack [202,0,4, 28,13,50,121
                                    ,140,113,187,56, 36,119,62,207
                                    ,24,107,72,153, 117,47,171,46
                                    ,51,128]
                  decoded = BS.unpack $ decode encoded
                  expected = [24,107,72,153, 140,113,187,56
                             ,36,119,62,207, 28,13,50,121
                             ,117,47,171,46]
              in do decoded @?= expected
                 


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

test_encode = let input = BS.pack [0,0,0,1, 0,0,0,2, 0,0,0,3, 0,0,0,1]
                  -- huff: 1 => 1, 01 => 3, 00 => 2
                  huffbits = [160] -- 10100000
                  vals = [0,0,0,1, 0,0,0,3, 0,0,0,2]
                  encodedvals = [140] -- 100011
                  off = [2]
                  combined = BS.pack $ huffbits ++ off ++ vals ++ encodedvals
              in do encode input @?= combined

test_encode2 = let input = BS.pack [0,0,0,1]
                   -- huff: 1 => 1
                   huffbits = [0] -- 00000000
                   vals = [0,0,0,1]
                   encodedvals = [128] -- 1
                   off = [7]
                   combined = BS.pack $ huffbits ++ off ++ vals ++ encodedvals
               in do encode input @?= combined

test_encode3 = let input = BS.pack [0,0,0,1, 0,0,0,1]
                   -- huff: 1 => 1
                   huffbits = [0] -- 00000000
                   vals = [0,0,0,1]
                   encodedvals = [192] -- 1
                   off = [6]
                   combined = BS.pack $ huffbits ++ off ++ vals ++ encodedvals
               in do encode input @?= combined

test_encode4 = let input = BS.pack [15,15,13,0,5,2,13,9,6,14,11,15]
                   -- huff: 15..=> 01, 5.. => 00, 6.. => 1
                   huffbits = [160] -- 10100
                   vals = [6,14,11,15,15,15,13,0,5,2,13,9]
                   encodedvals = [72] -- 01001
                   off = [3]
                   combined = huffbits ++ off ++ vals ++ encodedvals
               in do BS.unpack (encode input) @?= combined


testencodevalues = let hmap = M.fromList [(257, B.singleton False)]
                       vals = [257]
                       bits = [False]
                       hufftree = E.HuffTree hmap vals bits
                       rawbytes = BS.pack [0, 0, 1, 1]
                       (off, bytes) = encodevals hufftree rawbytes
                   in do BS.unpack bytes @?= [0]
                         off @?= 7

testencodevalues2 = let builder1 = B.singleton True
                        builder2 = B.singleton False `mappend` B.singleton True
                        builder3 = B.singleton False `mappend` B.singleton False
                        hmap = M.fromList [ (84020489, builder1)
                                          , (252644608, builder2)
                                          ,  (101583631, builder3)]
                        bits = [True, False, True, False, False]
                        vals = [ 84020489, 252644608, 101583631 ]
                        hufftree = E.HuffTree hmap vals bits
                        rawbytes = BS.pack [15,15,13,0,5,2,13,9,6,14,11,15]
                        (off, bytes) = encodevals hufftree rawbytes
                    in do BS.unpack bytes @?= [96]
                          off @?= 3

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

decode_huff2 = let hmap = M.fromList [("1",16778248), ("0",84214021)]
                   bytes = BS.pack [64] 
                   expected = [5,5,1,5,1,0,4,8]
               in do BS.unpack (decodeHuff hmap bytes 6) @?= expected

testdecode = let bits = BS.pack [160] 
                 off = BS.pack [1]
                 -- 1 => 42, 01 => 137, 00 => 111
                 vals = BS.pack [0, 0, 0, 42, 0, 0, 0, 137, 0, 0, 0, 111]
                 
                 -- 1010010110100000
                 encoded = BS.pack [165, 160]
                 combined = bits `mappend` off `mappend` vals `mappend` encoded
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
