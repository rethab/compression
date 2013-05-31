{-# LANGUAGE TemplateHaskell #-}

import Control.Monad (liftM, liftM2)
import Test.QuickCheck
import Test.QuickCheck.All
import Test.HUnit
import Huffman
import Tree

import qualified Data.ByteString.Lazy as BS

runTests :: IO Bool
runTests = $quickCheckAll

main = runTests >>= \passed -> if passed then putStrLn "All tests passed"
                                         else putStrLn "Some tests failed"

instance Arbitrary C.ByteString where
    arbitrary = fmap C.pack arbitrary

main = putStrLn "parse_props:" >>mapM_ quickCheck parse_props >> 
       putStrLn "prob_props:" >>mapM_ quickCheck prob_props >> 
       putStrLn "parse_props:" >>mapM_ quickCheck parse_props >> 
       mapM_ runTestTT units

-- QuickCheck: Binary Parser
parse_props = [prop_first_four, prop_consume_bytes]

prop_first_four bs = BS.length bs >= 4 ==>
    let ((Right out), _) = runGet parse32 bs
        (Just (exp, _))  = readWord32 bs
    in out == exp

prop_consume_bytes bs = BS.length bs >= 4 ==>
    let ((Right _), bs') = runGet parse32 bs
    in bs' /= bs

-- QuickCheck: Probabilities
prob_props = [prop_contains_all]

prop_contains_all bs =
    let ps = probs bs
    in map (flip $ M.member ps) words

-- QuickCheck: Parse
parse_props = [prop_first_ident, prop_len_mod4]

prop_first_ident bs = BS.length bs >= 4 ==>
    let out = parse bs !! 0
        ((Right exp), bs') = runGet parse32 bs
    in out == exp

grop_len_mod4 bs = 
    let out = parse bs !! 0
        len = BS.length bs
    in length out == (len - (len `mod` 4)) / 4

-- HUnit
units = map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("props_one_entry", TestCase probs_one_entry)
                   , ("probs_two_entries", TestCase probs_two_entries)
                   , ("probs_two_entries2", TestCase probs_two_entries2)
                   , ("probs_three_entries", TestCase probs_three_entries)
                   , ("probs_three_entries2", TestCase probs_three_entries2)
                   , ("probs_three_entries3", TestCase probs_three_entries3)
                   , ("probs_four_entries", TestCase probs_three_entries)
                   , ("probs_four_entries1", TestCase probs_three_entries1)
                   , ("probs_four_entries2", TestCase probs_three_entries2)
                   , ("probs_four_entries3", TestCase probs_three_entries3)
                   , ("probs_four_entries4", TestCase probs_three_entries4)
                   , ("probs_four_entries5", TestCase probs_three_entries5)
                   , ("probs_four_entries6", TestCase probs_three_entries6)
                   ]

-- HUnit: Probabilities
probs_one_entry = let ps = probs (to32BS [1])
                      Just val = M.lookup 1
                  in do val @?= 1

probs_two_entries = let ps = probs (to32BS [1, 2])
                        Just a = M.lookup 1
                        Just b = M.lookup 2
                    in do a @?= 0.5
                          b @?= 0.5

probs_two_entries2 = let ps = probs (to32BS [1, 1])
                         Just a = M.lookup 1
                     in do a @?= 1

probs_three_entries = let ps = probs (to32BS [1, 2, 3])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                          Just c = M.lookup 3
                      in do a @?= (1/3)
                            b @?= (1/3)
                            c @?= (1/3)

probs_three_entries2 = let ps = probs (to32BS [1, 2, 1])
                           Just a = M.lookup 1
                           Just b = M.lookup 2
                       in do a @?= (2/3)
                             b @?= (1/3)

probs_three_entries3 = let ps = probs (to32BS [1, 1, 1])
                           Just a = M.lookup 1
                       in do a @?= 1

probs_four_entries = let ps = probs (to32BS [1, 1, 1, 1])
                         Just a = M.lookup 1
                     in do a @?= 1

probs_four_entries2 = let ps = probs (to32BS [1, 1, 1, 2])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                      in do a @?= 0.75
                            b @?= 0.25

probs_four_entries3 = let ps = probs (to32BS [1, 1, 2, 2])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                      in do a @?= 0.5
                            b @?= 0.5

probs_four_entries4 = let ps = probs (to32BS [1, 2, 2, 2])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                      in do a @?= 0.25
                            b @?= 0.75

probs_four_entries5 = let ps = probs (to32BS [1, 2, 3, 2])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                          Just c = M.lookup 3
                      in do a @?= 0.25
                            b @?= 0.5
                            c @?= 0.25

probs_four_entries6 = let ps = probs (to32BS [1, 4, 3, 2])
                          Just a = M.lookup 1
                          Just b = M.lookup 2
                          Just c = M.lookup 3
                          Just d = M.lookup 4
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
