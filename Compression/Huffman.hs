module Compression.Huffman where

import Data.Word (Word32, Word8)
import Compression.Huffman.Encoder
import Compression.Huffman.Decoder

import qualified Data.Binary.Strict.Get    as Bin
import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.BitBuilder    as Builder
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString           as S
import qualified Data.HashMap.Strict       as Map

-- | A Word associated with its probability
type Probabilities = Map.HashMap Word32 Float

-- | A sequence of bits associated with its decoded value
type Decoder = Map.HashMap Bits Word32
type Bits = [Bool]

parse32 :: Bin.Get Word32
parse32 = Bin.getWord32be

-- | Encodes the 'BinaryString' with Huffmann
encode :: S.ByteString -> S.ByteString
encode bs = toStrictByteString $ foldl appendcode Builder.empty (parse bs)
    where huff = createHuffTree (Map.toList $ probs bs)
          appendcode acc word =
              case lookupBits huff word of
                Nothing -> error ("Word " ++ show word ++ "missing in Huffmann tree")
                Just bits -> Builder.append acc bits 
          toStrictByteString :: Builder.BitBuilder -> S.ByteString
          toStrictByteString = L.toStrict . Builder.toLazyByteString

decode :: S.ByteString -> S.ByteString
decode bs = decodeHuff decoder bs'
    where (decoder, bs') = readHuff bs

decodeHuff :: Decoder -> S.ByteString -> S.ByteString
decodeHuff dec bs = (S.pack . splitUp) (readValues dec bs)
    where splitUp :: [Word32] -> [Word8]
          splitUp = undefined

-- | Reads a Huffmann tree from the 'ByteString' and returns the
--   Huffmann-Tree  along with the rest of the 'ByteString'
readHuff :: S.ByteString -> (Decoder, S.ByteString)
readHuff = undefined

readValues :: Decoder -> S.ByteString -> [Word32]
readValues dec bs = case BitGet.runBitGet bs (readBits dec) of
                     Right (_, vals) -> vals
                     Left err -> error ("Failed to parse: " ++ err)

-- | Uses the decoder to read a Value and returns the number of bits consumed
readBits :: Decoder -> BitGet.BitGet (Int, [Word32])
readBits dec = go [] 0 []
    where go bitsacc nbits words =
            do done <- BitGet.isEmpty
               if done -- we might have undecoded bits in 'bitsacc'
                 then if null bitsacc then return (nbits, words) else fail "unconsumed bits"
                 else do bit <- BitGet.getBit
                         let bits' = bitsacc ++ [bit]
                         case Map.lookup bits' dec of
                           Nothing -> go bits' nbits words
                           Just word -> go [] (nbits + length bits') (words ++ [word])

-- | Finds the probabilities of all elements in the 'ByteString'
probs :: S.ByteString -> Probabilities
probs bs = let words = parse bs
               occurences = foldl add Map.empty words
               add map word = Map.insertWith (+) word 1 map
               elems = fromIntegral (length words)
           in Map.map (/elems) occurences

parse :: S.ByteString -> [Word32]
parse bs = case Bin.runGet parse32 bs of
             (Right word, bs') -> word : parse bs'
             (Left _, _) -> []
