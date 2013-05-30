import Criterion.Main 
import LZW (compress)
import Data.ByteString.Lazy.Char8 as B

main =
    do input <- B.readFile "big.txt"
       defaultMain [
           bench "big.txt" $ nf compress input
        ]
