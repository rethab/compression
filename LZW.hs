module LZW where

import Data.Bits                    ((.|.), shiftL)
import Data.Char                    (chr)
import Data.Monoid                  (mempty, mappend)
import Data.Word                    (Word8, Word32)

import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.Builder as B

-- | Compresses the passed 'ByteString' with the LZW algorithm
--
-- Algorithm:
--   while bytestring is not empty:
--     1. read char into accumulator
--     2. check if it exists in dictionary
--       a. not in dictionary: put previous index to builder
--                             goto 2 with last consumed char
--       b. in dictionary: remember index
--                         goto 1 with accumulator
--   if accumulator is not empty: (all previously consumed chars were in dict)
--                                put previous index to builder
--   else all done
compress :: BS.ByteString -> BS.ByteString
compress bs = B.toLazyByteString $ step defaultDict bs [] (-1)
    where
          -- take one char from bytestring and pass it to the encoding function
          step :: Dict -> C.ByteString -> String -> Int -> B.Builder
          step dict bs acc idx = case C.uncons bs of
                                   Nothing -> if null acc
                                                 -- end of input, no bytes consumed
                                                 then mempty 
                                                 -- end of input but we've begun searching
                                                 else B.word32BE (fromIntegral idx)
                                   Just (c, bs') -> encode dict bs' (acc, c) idx
          -- take consumed input (acc) and new char (c) and look it up in the dict
          encode :: Dict -> C.ByteString -> (String, Char) -> Int -> B.Builder
          encode dict bs (acc, c) idx = let acc' = acc ++ [c]
                                        in case search dict acc' of
                                             -- add old index to builder, encode consumed char
                                             Nothing -> mappend (B.word32BE $ fromIntegral idx)
                                                                (encode (put dict acc') bs ([], c) (-1))
                                             -- exists in dict. consume from bytestring
                                             Just idx' -> step dict bs acc' idx'

-- | Decompress the contents of the 'ByteString' with the LZW Algorithm
decompress :: BS.ByteString -> BS.ByteString
decompress bs = B.toLazyByteString $ step newdict bs []
    where step :: M.HashMap Word32 String -> BS.ByteString -> String -> B.Builder
          step dict bs prev = case readWord32 bs of
                                Nothing -> mempty
                                Just (idx, bs') -> flush dict bs' idx prev
          flush :: M.HashMap Word32 String -> BS.ByteString -> Word32 -> String -> B.Builder
          flush dict bs idx prev =
            case M.lookup idx dict of
              Nothing -> if idx /= dsize
                            then error ("missing index " ++ show idx)
                            else let suffix = take 1 prev
                                     cur = prev ++ suffix
                                 in mappend (B.string8 cur)
                                            (step (pushdict suffix) bs cur)
              Just word -> mappend (B.string8 word)
                                   (step (pushdict word) bs word)
              where dsize = fromIntegral $ M.size dict
                    pushdict suffix = if null prev
                                        then dict -- first lookup
                                        else M.insert dsize (prev ++ [head suffix]) dict
          newdict = M.fromList $ map (\ord -> (ord, chr' ord)) [0..255 :: Word32]
          chr' c = [chr (fromIntegral c)]

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

-- | The Keys are the actual entries, Values are the indices of the 'HashMap'
--
-- This ensures constant lookup of strings and constant put, because
-- the new index of a value (i.e. the actual value) will always be
-- the current size of the map.
--
-- The size is cached, because that is used on each insert (as the value)
-- and computing it would require O(n)
data Dict = Dict { size :: Int, vals :: M.HashMap String Int }

-- | Searches the given value in the dictionary and returns its index
search :: Dict -> String -> Maybe Int
search (Dict _ hmap) needle = M.lookup needle hmap 

-- | Appends to dictionary
put :: Dict -> String -> Dict
put (Dict hsize hmap) key = Dict { size = hsize + 1
                                 , vals = M.insert key hsize hmap }

-- | Default dictionary with ascii
defaultDict :: Dict
defaultDict = Dict { size = 256
                   , vals = M.fromList $ map (\ord -> (chr' ord, ord)) [0..255] }
    where chr' c = [chr c]
