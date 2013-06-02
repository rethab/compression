module Main where

import Control.Monad      (when)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.ByteString.Lazy as BS

import LZW                (compress, decompress)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) (usage >> exitFailure)
    let mod = args !! 0
        src = args !! 1
        dst = args !! 2
        trans = if mod == "c"
                  then compress
                  else decompress
    bs <- BS.readFile src
    BS.writeFile dst (trans bs)

usage :: IO ()
usage = putStrLn "lzw [c/d] source destination"

