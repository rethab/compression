import Control.Monad
import Debug.Trace (trace)
import Test.QuickCheck hiding ((.&.))
import Test.HUnit
import Data.Maybe
import Data.Bits
import Data.Word
import Data.Monoid
import LZW hiding (main)

import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Builder as B

instance Arbitrary C.ByteString where
    arbitrary = fmap C.pack arbitrary

-- | Convert a list of 32 bit words to a bytestring (word8 stream)
to32BS :: [Word32] -> BS.ByteString
to32BS = foldr BS.cons BS.empty . fill
    where fill [] = []
          fill (x:xs) = let a = fromIntegral (shiftR (0xFF000000 .&. x) 24)
                            b = fromIntegral (shiftR (0x00FF0000 .&. x) 16)
                            c = fromIntegral (shiftR (0x0000FF00 .&. x) 8)
                            d = fromIntegral (0x000000FF .&. x)
                        in a : b : c : d : fill xs

main = putStrLn "string_props:" >>mapM_ quickCheck string_props >> 
       putStrLn "word8_props:" >> mapM_ quickCheck word8_props >>  
       putStrLn "bytestring_props:" >> mapM_ quickCheck bytestring_props >> 
       putStrLn "bytestring_props2:" >> mapM_ quickCheck bytestring_props2 >> 
       mapM_ runTestTT units

-- QuickCheck Tests: String 
string_props = [ exist_after_put, only_ascii_exist ]

exist_after_put xs = length xs > 1 ==> let withVal = put defaultDict xs 
                                       in isJust (search withVal xs)

only_ascii_exist xs = not (null xs) ==> if length xs > 1
                                          then isNothing (search defaultDict xs)
                                          else isJust (search defaultDict xs)

-- QuickCheck Tests: ByteString
bytestring_props = [consumes_four]

consumes_four bs = BS.length bs >= 4 ==> let (w32, _) = fromJust $ readWord32 bs
                                             a = BS.unpack bs !! 0
                                             b = BS.unpack bs !! 1
                                             c = BS.unpack bs !! 2
                                             d = BS.unpack bs !! 3
                                             exp = to32 a b c d
                                          in w32 == exp

-- QuickCheck Tests: ByteString 2
bytestring_props2 = [ roundtrip ]

roundtrip bs = bs == decompress (compress bs)
                                             

-- QuickCheck Tests: Word8
word8_props = [ mod_last, multiplication ]

mod_last a b c d = (to32 a b c d .&. fromIntegral d)
                == fromIntegral d

multiplication a b c d = let act =  to32 a b c d
                             exp = ((fromIntegral a*2^24)
                                  + (fromIntegral b*2^16)
                                  + (fromIntegral c*2^8)
                                  + fromIntegral d)
                         in act == exp

-- HUnit Tests
units = map (\(lbl, test) -> TestLabel lbl test) units'
    where units' = [ ("append_index", TestCase append_index)
                   , ("append_index2", TestCase append_index2)
                   , ("c_star_sample", TestCase c_star_sample)
                   , ("c_one_char_s", TestCase c_one_char_s)
                   , ("c_one_char_d", TestCase c_one_char_d)
                   , ("c_one_char_t", TestCase c_one_char_t)
                   , ("c_one_char_q", TestCase c_one_char_q)
                   , ("c_two_chars_s", TestCase c_two_chars_s)
                   , ("c_two_chars_t", TestCase c_two_chars_t)
                   , ("c_three_chars_s", TestCase c_three_chars_s)
                   , ("c_four_chars_s", TestCase c_four_chars_s)
                   , ("c_dict_always", TestCase c_dict_always)
                   , ("d_star_sample", TestCase d_star_sample)
                   , ("d_one_char_s", TestCase d_one_char_s)
                   , ("d_one_char_d", TestCase d_one_char_d)
                   , ("d_one_char_t", TestCase d_one_char_t)
                   , ("d_one_char_q", TestCase d_one_char_q)
                   , ("d_two_chars_s", TestCase d_two_chars_s)
                   , ("d_two_chars_t", TestCase d_two_chars_t)
                   , ("d_three_chars_s", TestCase d_three_chars_s)
                   , ("d_four_chars_s", TestCase d_four_chars_s)
                   , ("d_dict_always", TestCase d_dict_always)
                   ]

-- Dict tests

append_index = let dict = put defaultDict "AA"
                   idx = fromJust (search dict "AA")
               in do idx @?= 256

append_index2 = let dict = put (put defaultDict "AA") "AAA"
                    idx1 = fromJust (search dict "AA")
                    idx2 = fromJust (search dict "AAA")
                in do idx1 @?= 256
                      idx2 @?= 257

-- Compression tests

c_star_sample = let inp = C.pack "BCAAABCACAAABBAAABCB"
                    out = to32BS [66, 67, 65, 258, 256, 65, 257, 259, 66, 258, 65, 256, 66]
              in do compress inp @?= out

c_one_char_s = let inp = C.pack "A"
                   out = to32BS [65]
             in do compress inp @?= out

c_one_char_d = let inp = C.pack "AA"
                   out = to32BS [65, 65]
             in do compress inp @?= out

c_one_char_t = let inp = C.pack "AAA"
                   out = to32BS [65, 256]
             in do compress inp @?= out

c_one_char_q = let inp = C.pack "AAAA"
                   out = to32BS [65, 256, 65]
             in do compress inp @?= out

c_two_chars_s = let inp = C.pack "AB"
                    out = to32BS [65, 66]
              in do compress inp @?= out

c_two_chars_t = let inp = C.pack "ABAB"
                    out = to32BS [65, 66, 256]
              in do compress inp @?= out

c_three_chars_s = let inp = C.pack "ABC"
                      out = to32BS [65, 66, 67]
                in do compress inp @?= out

c_four_chars_s = let inp = C.pack "ABCD"
                     out = to32BS [65, 66, 67, 68]
               in do compress inp @?= out

c_dict_always = let inp = C.pack "ABABCABCDABCDEABCDE"
                    out = to32BS [65, 66, 256, 67, 258, 68, 260, 69, 262]
              in do compress inp @?= out

-- Decompression Tests

d_star_sample = let out = C.pack "BCAAABCACAAABBAAABCB"
                    inp = to32BS [66, 67, 65, 258, 256, 65, 257, 259, 66, 258, 65, 256, 66]
              in do decompress inp @?= out

d_one_char_s = let out = C.pack "A"
                   inp = to32BS [65]
             in do decompress inp @?= out

d_one_char_d = let out = C.pack "AA"
                   inp = to32BS [65, 65]
             in do decompress inp @?= out

d_one_char_t = let out = C.pack "AAA"
                   inp = to32BS [65, 256]
             in do decompress inp @?= out

d_one_char_q = let out = C.pack "AAAA"
                   inp = to32BS [65, 256, 65]
             in do decompress inp @?= out

d_two_chars_s = let out = C.pack "AB"
                    inp = to32BS [65, 66]
              in do decompress inp @?= out

d_two_chars_t = let out = C.pack "ABAB"
                    inp = to32BS [65, 66, 256]
              in do decompress inp @?= out

d_three_chars_s = let out = C.pack "ABC"
                      inp = to32BS [65, 66, 67]
                in do decompress inp @?= out

d_four_chars_s = let out = C.pack "ABCD"
                     inp = to32BS [65, 66, 67, 68]
               in do decompress inp @?= out

d_dict_always = let out = C.pack "ABABCABCDABCDEABCDE"
                    inp = to32BS [65, 66, 256, 67, 258, 68, 260, 69, 262]
              in do decompress inp @?= out

