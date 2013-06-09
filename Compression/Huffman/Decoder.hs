module Compression.Huffman.Decoder where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Word     (Word8, Word16)

import qualified Data.Binary.Strict.BitGet as BitGet
import qualified Data.Binary.Strict.Get    as BinGet
import qualified Data.ByteString           as S
import qualified Data.HashMap.Strict       as M

data BTree = Node BTree BTree | Leaf deriving (Show, Eq)

deserialize :: S.ByteString -> (M.HashMap String Word16, Word8, S.ByteString)
deserialize bs = let (tree, bs') = decodeTree bs
                     (off, bs'') = read8 bs'
                     (dec, bs''') = makeDecoder tree bs''
                 in (dec, off, bs''')

read8 :: S.ByteString -> (Word8, S.ByteString)
read8 bs = let (res, bs') = BinGet.runGet BinGet.getWord8 bs
           in case res of
                Left e -> error e
                Right val -> (val, bs')

read32 :: S.ByteString -> (Word16, S.ByteString)
read32 bs = let (res, bs') = BinGet.runGet BinGet.getWord16be bs
             in case res of
                  Left e -> error e
                  Right val -> (val, bs')

makeDecoder :: BTree -> S.ByteString -> (M.HashMap String Word16, S.ByteString)
makeDecoder Leaf bs = first (M.singleton "1") (read32 bs)
makeDecoder tree bs = go tree bs [] M.empty
    where go Leaf bs code hmap = first (\x -> M.insert code x hmap) (read32 bs)
          go (Node l r) bs code hmap =
            let (lhmap, bs')  = go l bs (code++"1") hmap
                (rhmap, bs'') = go r bs' (code++"0") hmap
            in  (lhmap `M.union` rhmap, bs'')

decodeTree :: S.ByteString -> (BTree, S.ByteString)
decodeTree bs = case BitGet.runBitGet bs readTree of
                          Left e -> error e
                          Right (bits, tree) -> (tree, S.drop bytes bs)
                            where bytes = ceiling (fromIntegral bits / 8)

readTree :: BitGet.BitGet (Int, BTree)
readTree = do set <- BitGet.getBit
              if set
                  then do (lb, l) <- readTree
                          (rb, r) <- readTree
                          return (lb+rb+1, Node l r)
                  else return (1, Leaf)
