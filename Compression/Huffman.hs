{-
 - Author: Reto Hablützel (rethab)
 -
 -
 - This module encodes a 'ByteString' with the Huffman - Algorithm.
 -
 - The encoded structure is as follows:
 -  -  Stucture of the Huffman - Tree (k Bits)
 -  -  Offsetin last Byte (0-7 Bits)
 -  -  Values of all Leaves (n * m * 8 Bits, m = Bit/Number, n = Number of Values
 -  -  Encoded Values (x Bits)
 -
 -
 - 1. The structure of the tree is encoded as a sequence of bits whereby a '1'
 -    denotes a node and '0' denotes a leaf. The algorithm is roughly described as
 -    follows: read one bit, if it is a node, push it to a stack and dive on the left.
 -    Then read the next bit, if that is a leaf, you know it belongs to the the element
 -    on top of the stack whereas if it is a node, push that to the stack as well.
 -
 - 2. The offset in the last byte is required because we cannot know when we are done
 -    reading if we are inside a byte. imagine having a certain number that is encoded
 -    as '0' and then try to parse a it with a input of one byte with a value of zero.
 -    In this scenario, for instance, you cannot know whether these are eight values
 -    or only one and the rest was just filled up because we only have whole bytes.
 -
 - 3. Obviously, we want to reconstruct all the values from the encoded bit-sequence.
 -    This list of all values, will be assigned depth-first to all leaves of the tree.
 -
 - 4. All values are encoded in a sequence of bits. Codes can be of arbitrary size and
 -    in the last byte, an offset of bits is to be ignored (see 2.).
 -}
module Compression.Huffman where

import Control.Applicative ((<$>))
import Data.Monoid         (mappend)
import Data.Word           (Word8)

import qualified Data.Binary.Strict.Get    as Bin
import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.Put           as BinPut
import qualified Data.Binary.BitBuilder    as Builder
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString           as S
import qualified Data.HashMap.Strict       as M

import qualified Compression.Huffman.Encoder as E
import qualified Compression.Huffman.Decoder as D

-- | Encodes the 'BinaryString' with Huffmann
encode :: S.ByteString -> S.ByteString
encode bs = let wrds = S.unpack bs
                hufftree = E.createHuffTree (probs wrds)
                (off, encvals) = encodevals hufftree bs
                encodedtree = E.serializeTree hufftree off
            in encodedtree `mappend` encvals


encodevals :: E.HuffTree Word8 -> S.ByteString -> (Word8, S.ByteString)
encodevals huff bs =
             let builder = foldl (accbit huff) Builder.empty (S.unpack bs)
                 bytes = L.toStrict (Builder.toLazyByteString builder)
                 off = countRemaining builder
             in (off, bytes)

accbit huff acc word =
       case E.lookupBits huff word of
         Nothing -> error (show word ++ " not in Huffmann tree")
         Just bits -> Builder.append acc bits 

countRemaining :: Builder.BitBuilder -> Word8
countRemaining builder = go builder 0 
    where go b n 
             | bsl (appenddummy b) /= origLen = n
             | otherwise = go (appenddummy b) (n+1)
          appenddummy = mappend (Builder.singleton False)
          builderlen builder' = L.length (Builder.toLazyByteString builder')
          origLen = builderlen builder
          bsl builder' = builderlen builder'

decode :: S.ByteString -> S.ByteString
decode bs = decodeHuff decoder bs' (fromIntegral off)
    where (decoder, off, bs') = readHuff bs

decodeHuff :: M.HashMap String Word8 -> S.ByteString -> Int -> S.ByteString
decodeHuff dec bs off = let (vals, _) = readValues dec bs off
                    in toBS vals
    where toBS :: [Word8] -> S.ByteString
          toBS = S.concat . map (L.toStrict . BinPut.runPut . BinPut.putWord8)

-- | Reads a Huffmann tree from the 'ByteString' and returns the
--   Huffmann-Tree  along with the rest of the 'ByteString'
readHuff :: S.ByteString -> (M.HashMap String Word8, Word8, S.ByteString)
readHuff = D.deserialize

readValues :: M.HashMap String Word8
           -> S.ByteString
           -> Int
           -> ([Word8], S.ByteString)
readValues dec bs off =
    case BitGet.runBitGet bs (readBits dec off) of
      Right (nbits, vals) -> (vals, S.drop (nbytes nbits) bs)
      Left err -> error ("Failed to parse: " ++ err)
    where nbytes nbits = ceiling (fromIntegral nbits / 8)

-- | Uses the decoder to read a Value and returns the number of bits consumed.
--   in the last byte, 'offset' bits are discarded
readBits :: M.HashMap String Word8 -> Int -> BitGet.BitGet (Int, [Word8])
readBits dec offset = go [] 0 []
    where go bitsacc nbits wrds =
            do done <- BitGet.isEmpty
               remaining <- BitGet.remaining
               if remaining == offset
                    -- discard following
                  || done
                    -- we might have undecoded bits in 'bitsacc'
                    -- but leaves since they probably come from 
                    -- stuffing
                 then return (nbits, wrds) 
                 else do bit <- BitGet.getBit
                         let bits' = bitsacc ++ [b2c bit]
                         case M.lookup bits' dec of
                           Nothing -> go bits' nbits wrds
                           Just word -> go [] (nbits + length bits') (wrds ++ [word])
          b2c True  = '1'
          b2c False = '0'

-- | Finds the probabilities of all elements in the 'ByteString'
probs :: [Word8] -> [(Word8, Float)]
probs wrds = let occurences = foldl add M.empty wrds
                 add map word = M.insertWith (+) word 1 map
                 elems = fromIntegral (length wrds)
             in M.toList (M.map (/elems) occurences)
