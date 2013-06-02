module Compression.Main where

import Control.Monad      (when)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.ByteString.Lazy as BS

import Compression.LZW (compress, decompress)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) (usage >> exitFailure)
    let mode = args !! 0
        src = args !! 1
        dst = args !! 2
        trans = if mode == "c"
                  then compress
                  else decompress
    bs <- BS.readFile src
    BS.writeFile dst (trans bs)

usage :: IO ()
usage = putStrLn "lzw [c/d] source destination"

