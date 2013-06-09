module Main where

import Control.Monad      (when)
import Data.Maybe         (isNothing, fromJust)
import System.Environment (getArgs)
import System.Exit        (exitFailure)

import qualified Data.ByteString as BS

import Compression.LZW (compress, decompress)
import Compression.Huffman (encode, decode)

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 3) (usage >> exitFailure)
    let mode = args !! 0
        src = args !! 1
        dst = args !! 2
        mbtrans = lookup mode opts
    when (isNothing mbtrans) usage
    bs <- BS.readFile src
    BS.writeFile dst ((fromJust mbtrans) bs)

usage :: IO ()
usage = do putStrLn "[mode] source destination"
           let mods = map fst opts
           putStrLn " where mode is one of:"
           putStrLn $ "   " ++ show mods


opts = [("enc", encode), ("comp", compress), ("encomp", encode . compress)
       ,("dec", decode), ("decomp", decompress), ("deccomp", decompress . decode)]
