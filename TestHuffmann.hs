{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (liftM, liftM2)
import Data.Bits
import Data.Word
import Debug.Trace   (trace)

import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck.All
import Test.HUnit

import Huffmann

import qualified Data.Binary.Strict.Get as BinStrict
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as M

main = mapM_ runTestTT units >> $quickCheckAll

instance Arbitrary BS.ByteString where
    arbitrary = fmap BS.pack arbitrary

prop_first_four bs = BS.length bs >= 4 ==>
    let ((Right out), _) = BinStrict.runGet parse32 bs
        (Just (exp, _))  = readWord32 bs
    in out == exp

prop_consume_bytes bs = BS.length bs >= 4 ==>
    let ((Right _), bs') = BinStrict.runGet parse32 bs
    in bs' /= bs

prop_prob_sum_is_one bs = BS.length bs >= 4 ==>
    let ps = probs bs
        sm = M.foldl' (+) 0 ps 
    in (abs (sm - 1)) < 0.001

prop_contains_all bs =
    let ps = probs bs
        words = parse bs
    in all (\word -> M.member word ps) words

prop_first_ident bs = BS.length bs >= 4 ==>
    let out = parse bs !! 0
        ((Right exp), _) = BinStrict.runGet parse32 bs
    in out == exp

grop_len_mod4 bs = 
    let out = parse bs
        len = BS.length bs
    in fromIntegral (length out) == (fromIntegral (len - (len `mod` 4))) / 4

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
                   ]

-- HUnit: Probabilities
probs_one_entry = let ps = probs (to32BS [1])
                      Just val = M.lookup 1 ps
                  in do val @?= 1

probs_two_entries = let ps = probs (to32BS [1, 2])
                        Just a = M.lookup 1 ps
                        Just b = M.lookup 2 ps
                    in do a @?= 0.5
                          b @?= 0.5

probs_two_entries2 = let ps = probs (to32BS [1, 1])
                         Just a = M.lookup 1 ps
                     in do a @?= 1

probs_three_entries = let ps = probs (to32BS [1, 2, 3])
                          Just a = M.lookup 1 ps
                          Just b = M.lookup 2 ps
                          Just c = M.lookup 3 ps
                      in do a @?= (1/3)
                            b @?= (1/3)
                            c @?= (1/3)

probs_three_entries2 = let ps = probs (to32BS [1, 2, 1])
                           Just a = M.lookup 1 ps
                           Just b = M.lookup 2 ps
                       in do a @?= (2/3)
                             b @?= (1/3)

probs_three_entries3 = let ps = probs (to32BS [1, 1, 1])
                           Just a = M.lookup 1 ps
                       in do a @?= 1

probs_four_entries = let ps = probs (to32BS [1, 1, 1, 1])
                         Just a = M.lookup 1 ps
                     in do a @?= 1

probs_four_entries2 = let ps = probs (to32BS [1, 1, 1, 2])
                          Just a = M.lookup 1 ps
                          Just b = M.lookup 2 ps
                      in do a @?= 0.75
                            b @?= 0.25

probs_four_entries3 = let ps = probs (to32BS [1, 1, 2, 2])
                          Just a = M.lookup 1 ps
                          Just b = M.lookup 2 ps
                      in do a @?= 0.5
                            b @?= 0.5

probs_four_entries4 = let ps = probs (to32BS [1, 2, 2, 2])
                          Just a = M.lookup 1 ps 
                          Just b = M.lookup 2 ps 
                      in do a @?= 0.25
                            b @?= 0.75

probs_four_entries5 = let ps = probs (to32BS [1, 2, 3, 2])
                          Just a = M.lookup 1 ps
                          Just b = M.lookup 2 ps
                          Just c = M.lookup 3 ps
                      in do a @?= 0.25
                            b @?= 0.5
                            c @?= 0.25

probs_four_entries6 = let ps = probs (to32BS [1, 4, 3, 2])
                          Just a = M.lookup 1 ps 
                          Just b = M.lookup 2 ps
                          Just c = M.lookup 3 ps
                          Just d = M.lookup 4 ps
                      in do a @?= 0.25
                            b @?= 0.25
                            c @?= 0.25
                            d @?= 0.25

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

to32BS :: [Word32] -> BS.ByteString
to32BS = foldr BS.cons BS.empty . fill
    where fill [] = []
          fill (x:xs) = let a = fromIntegral (shiftR (0xFF000000 .&. x) 24)
                            b = fromIntegral (shiftR (0x00FF0000 .&. x) 16)
                            c = fromIntegral (shiftR (0x0000FF00 .&. x) 8)
                            d = fromIntegral (0x000000FF .&. x)
                        in a : b : c : d : fill xs
