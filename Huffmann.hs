module Huffmann where

import Data.Word

import qualified Data.Binary.Strict.Get as BinStrict
import qualified Data.ByteString        as BS
import qualified Data.HashMap.Strict    as Map

type Probabilities = Map.HashMap Word32 Float



parse32 :: BinStrict.Get Word32
parse32 = BinStrict.getWord32be

-- | Encodes the 'BinaryString' with Huffmann
encode :: BS.ByteString -> BS.ByteString
encode = undefined

-- | Finds the probabilities of all elements in the 'ByteString'
probs :: BS.ByteString -> Probabilities
probs bs = let words = parse bs
               add map word = Map.insertWith (+) word 1 map
               occurences = foldl add Map.empty words
               elems = length words
           in Map.map (\occ -> elems / occ) occurences



-- | Replaces the contents of the 'ByteString' with it
--   encoded values from the 'HashMap'
replace :: Probabilities -> BS.ByteString -> BS.ByteString
replace = undefined

parse :: BS.ByteString -> [Word32]
parse bs = case runGet parse32 bs of
             (Right word, bs') -> word : parse bs'
             (Left _, _) -> []
