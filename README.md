# Compressor
Currently supports LZW and Huffman

## Benchs (all values in bytes)
Original File Size: 10463 

LZW uses 32 bit sized entries in the tables, which probably is somewhat suboptimal.

# 32 Bit Huffman
- Huffman only: 11433
- LZW, then Huffman: 12181

# 16 Bit Huffman
- Huffman only: 6713
- LZW, then Huffman: 9857

# 8 Bit Huffman
- Huffman only: 6286
- LZW, then Huffman: 8449
