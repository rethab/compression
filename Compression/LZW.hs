module Compression.LZW where

import Data.Bits                    ((.|.), shiftL)
import Data.Char                    (chr)
import Data.Hashable                (Hashable)
import Data.Monoid                  (mempty, mappend)
import Data.Word                    (Word, Word8, Word16)
import Debug.Trace                  (trace)

import qualified Data.HashMap.Lazy as M
import qualified Data.ByteString      as S
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
compress :: S.ByteString -> S.ByteString
compress = BS.toStrict . B.toLazyByteString . step defaultCDict [] (-1) . BS.fromStrict
    where
          -- take one char from bytestring and pass it to the encoding function
          step :: Dict String Word -> String -> Word -> C.ByteString -> B.Builder
          step dict acc idx bs = case C.uncons bs of
                                   Nothing -> if null acc
                                                 -- end of input, no bytes consumed
                                                 then mempty
                                                 -- end of input but we've begun searching
                                                 else B.word16BE (fromIntegral idx)
                                   Just (c, bs') -> encode dict bs' (acc, c) idx
          -- take consumed input (acc) and new char (c) and look it up in the dict
          encode :: Dict String Word -> C.ByteString -> (String, Char) -> Word -> B.Builder
          encode dict bs (acc, c) idx = let acc' = acc ++ [c]
                                         in case search dict acc' of
                                             -- add old index to builder, encode consumed char
                                             Nothing -> mappend (B.word16BE $ fromIntegral idx)
                                                                (encode (putC dict acc') bs ([], c) (-1))
                                             -- exists in dict. consume from bytestring
                                             Just idx' -> step dict acc' idx' bs 

-- | Decompress the contents of the 'ByteString' with the LZW Algorithm
decompress :: S.ByteString -> S.ByteString
decompress = BS.toStrict . B.toLazyByteString . step defaultDDict [] . BS.fromStrict
    where step :: Dict Word16 String -> String -> BS.ByteString -> B.Builder
          step dict prev bs = case readWord16 bs of
                                Nothing -> mempty
                                Just (idx, bs') -> flush dict bs' idx prev
          flush :: Dict Word16 String -> BS.ByteString -> Word16 -> String -> B.Builder
          flush dict bs idx prev =
            case search dict idx of
              Nothing -> if idx /= fromIntegral (dsize dict)
                            then error ("missing index " ++ show idx)
                            else let suffix = take 1 prev
                                     cur = prev ++ suffix
                                 in mappend (B.string8 cur)
                                            (step (pushdict suffix) cur bs)
              Just word -> mappend (B.string8 word)
                                   (step (pushdict word) word bs)
              where dsize (Dict size' _ ) = size'
                    pushdict suffix = if null prev
                                        then dict -- first lookup
                                        else putD dict (prev ++ [head suffix])

-- | Reads four 'Word8's and puts them together into a 'Word16'
readWord16 :: BS.ByteString -> Maybe (Word16, BS.ByteString)
readWord16 bs = do (a, bs')    <- BS.uncons bs
                   (b, bs'')   <- BS.uncons bs'
                   Just (to16 a b, bs'')

-- | Puts 4 'Word8's together to a 'Word16'
to16 :: Word8 -> Word8 -> Word16
to16 a b = shiftL (fromIntegral a)  8 .|. (fromIntegral b)

-- | Dictionary type for LZW algorithm. The internal structure is a
--   HashMap for constant lookup and constant insert. Since, depending
--   on whether we're compressing or decompressing, the current size
--   of the hashmap is the key or the value, we cache the size as
--   evaluating it has complexity of O(n)
data Dict k v = Dict { size :: Word, vals :: M.HashMap k v }

-- | Searches the given value in the dictionary and returns its index
search :: (Hashable k, Eq k) => Dict k v -> k -> Maybe v
search (Dict _ hmap) needle = M.lookup needle hmap 

-- | Appends to dictionary for compression
putC :: Dict String Word -> String -> Dict String Word
putC (Dict hsize hmap) key = Dict { size = hsize + 1
                                  , vals = M.insert key hsize hmap }

-- | Appends to dictionary for decompression
putD :: Dict Word16 String -> String -> Dict Word16 String
putD (Dict hsize hmap) val = Dict { size = hsize + 1
                                  , vals = M.insert (fromIntegral hsize) val hmap }

-- | Default dictionary for compression with ascii
defaultCDict :: Dict String Word
defaultCDict = Dict { size = 256
                   , vals = M.fromList $ map (\ord -> (chr' ord, ord)) [0..255] }
    where chr' c = [chr $ fromIntegral c]

-- | Default dictionary for decompression with ascii
defaultDDict :: Dict Word16 String
defaultDDict = Dict { size = 256
                   , vals = M.fromList $ map (\ord -> (ord, chr' ord)) [0..255 :: Word16] }
    where chr' c = [chr $ fromIntegral c]
