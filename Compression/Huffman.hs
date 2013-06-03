module Compression.Huffman where

import Control.Applicative ((<$>))
import Data.Monoid         (mappend)
import Data.Word           (Word32, Word8)

import qualified Data.Binary.Strict.Get    as Bin
import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.Put           as BinPut
import qualified Data.Binary.BitBuilder    as Builder
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString           as S
import qualified Data.HashMap.Strict       as M

import qualified Compression.Huffman.Encoder as E
import qualified Compression.Huffman.Decoder as D

parse32 :: Bin.Get Word32
parse32 = Bin.getWord32be

-- | Encodes the 'BinaryString' with Huffmann
encode :: S.ByteString -> S.ByteString
encode bs = let words = parse bs
                hufftree = E.createHuffTree (probs words)
                encodedvalues = encodevals hufftree bs
                encodedtree = E.serializeTree hufftree
            in encodedtree `mappend` encodedvalues

encodevals :: E.HuffTree Word32 -> S.ByteString -> S.ByteString
encodevals huff bs = let builder = foldl accbit Builder.empty (parse bs)
             in L.toStrict $ Builder.toLazyByteString builder
    where accbit acc word =
            case E.lookupBits huff word of
              Nothing -> error (show word ++ " not in Huffmann tree")
              Just bits -> Builder.append acc bits 

decode :: S.ByteString -> S.ByteString
decode bs = decodeHuff decoder bs'
    where (decoder, bs') = readHuff bs

decodeHuff :: M.HashMap String Word32-> S.ByteString -> S.ByteString
decodeHuff dec bs = toBS (readValues dec bs)
    where toBS :: [Word32] -> S.ByteString
          toBS = S.concat . map (L.toStrict . BinPut.runPut . BinPut.putWord32be)

-- | Reads a Huffmann tree from the 'ByteString' and returns the
--   Huffmann-Tree  along with the rest of the 'ByteString'
readHuff :: S.ByteString -> (M.HashMap String Word32, S.ByteString)
readHuff = D.deserialize

readValues :: M.HashMap String Word32 -> S.ByteString -> [Word32]
readValues dec bs = case BitGet.runBitGet bs (readBits dec) of
                     Right (_, vals) -> vals
                     Left err -> error ("Failed to parse: " ++ err)

-- | Uses the decoder to read a Value and returns the number of bits consumed
readBits :: M.HashMap String Word32 -> BitGet.BitGet (Int, [Word32])
readBits dec = go [] 0 []
    where go bitsacc nbits words =
            do done <- BitGet.isEmpty
               if done -- we might have undecoded bits in 'bitsacc'
                 then if null bitsacc then return (nbits, words) else fail "unconsumed bits"
                 else do bit <- BitGet.getBit
                         let bits' = bitsacc ++ [b2c bit]
                         case M.lookup bits' dec of
                           Nothing -> go bits' nbits words
                           Just word -> go [] (nbits + length bits') (words ++ [word])
          b2c True  = '1'
          b2c False = '0'

-- | Finds the probabilities of all elements in the 'ByteString'
probs :: [Word32] -> [(Word32, Float)]
probs words = let occurences = foldl add M.empty words
                  add map word = M.insertWith (+) word 1 map
                  elems = fromIntegral (length words)
              in M.toList (M.map (/elems) occurences)

parse :: S.ByteString -> [Word32]
parse bs = case Bin.runGet parse32 bs of
             (Right word, bs') -> word : parse bs'
             (Left _, _) -> []
