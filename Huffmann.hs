module Huffmann where

import Data.Word
import Tree (lookupBits, toHuffTree)

import qualified Data.Binary.Get        as Bin
import qualified Data.Binary.BitBuilder as B
import qualified Data.ByteString.Lazy   as BS
import qualified Data.HashMap.Strict    as Map

type Probabilities = Map.HashMap Word32 Float

parse32 :: Bin.Get Word32
parse32 = Bin.getWord32be

-- | Encodes the 'BinaryString' with Huffmann
encode :: BS.ByteString -> BS.ByteString
encode bs = B.toLazyByteString $ foldl appendcode B.empty (parse bs)
    where huff = toHuffTree (Map.toList $ probs bs)
          appendcode acc word =
              case lookupBits huff word of
                Nothing -> error ("Word " ++ show word ++ "missing in Huffmann tree")
                Just bits -> B.append acc bits 

-- | Finds the probabilities of all elements in the 'ByteString'
probs :: BS.ByteString -> Probabilities
probs bs = let words = parse bs
               occurences = foldl add Map.empty words
               add map word = Map.insertWith (+) word 1 map
               elems = fromIntegral (length words)
           in Map.map (/elems) occurences

-- | Replaces the contents of the 'ByteString' with it
--   encoded values from the 'HashMap'
replace :: Probabilities -> BS.ByteString -> BS.ByteString
replace = undefined

parse :: BS.ByteString -> [Word32]
parse bs = case Bin.runGet parse32 bs of
             (Right word, bs') -> word : parse bs'
             (Left _, _) -> []
