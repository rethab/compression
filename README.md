# Compressor
Currently supports LZW and Huffman

## Benchs (all values in bytes)
Original File Size: 10463 

# 32 Bit Huffman
- Huffman only: 11433
- LZW (32 Bit), then Huffman: 12181

# 16 Bit Huffman
- Huffman only: 6713
- LZW (32 Bit), then Huffman: 9857

# 8 Bit Huffman
- Huffman only: 6286
- LZW (32 Bit), then Huffman: 8449
- LZW (16 Bit), then Huffman: 6858

# LZW 16 Bit
- LZW (16 Bit): 8102
