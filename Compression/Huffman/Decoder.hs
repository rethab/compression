module Compression.Huffman.Decoder where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Word     (Word32, Word64)

import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.Strict.Get    as BinGet
import qualified Data.ByteString           as S
import qualified Data.HashMap.Strict       as M

data BTree = Node BTree BTree | Leaf deriving (Show)

deserialize :: S.ByteString -> (M.HashMap String Word32, S.ByteString)
deserialize bs = let (nleaves, bs') = (first fromIntegral) $ read64 bs
                     (leaves, bs'') = read32 bs' nleaves
                     decoder = readDecoder bs'' leaves
                     -- assumption: a binary tree (not necessarily balanced)
                     -- with n leaves has (n-1) nodes. thus the number of bits
                     -- read is the sum of the two
                     nbytes = ceiling $ (fromIntegral (nleaves + (nleaves - 1))) / 8
                 in (decoder, S.drop nbytes bs'')

read32 :: S.ByteString -> Int -> ([Word32], S.ByteString)
read32 bs 0 = ([], bs)
read32 bs n = let (res, bs') = BinGet.runGet BinGet.getWord32be bs
              in case res of
                   Left e -> error e
                   Right val -> first (val:) (read32 bs' (n-1))

read64 :: S.ByteString -> (Word64, S.ByteString)
read64 bs = let (res, bs') = BinGet.runGet BinGet.getWord64be bs
            in case res of
                 Left e -> error e
                 Right val -> (val, bs')

readDecoder :: S.ByteString -> [Word32] -> M.HashMap String Word32
readDecoder bs leaves = case BitGet.runBitGet bs readTree of
                          Left e -> error e
                          Right tree -> mkDecoder tree leaves

mkDecoder :: BTree -> [Word32] -> M.HashMap String Word32
mkDecoder Leaf [val] = M.singleton "1" val
mkDecoder root values = snd $ go root values [] M.empty
    where go Leaf vals code map = (1, M.insert code (head vals) map)
          go (Node l r) vals code map =
            let (nleavesl, map') = go l vals (code++"1") map
                (nleavesr, map'')= go r (drop nleavesl vals) (code++"0") map'
            in (nleavesl+nleavesr, map'')

readTree :: BitGet.BitGet BTree
readTree = BitGet.getBit >>= \set ->
    if set then liftM2 Node readTree readTree else return Leaf
