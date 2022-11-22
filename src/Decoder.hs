{-# LANGUAGE BinaryLiterals #-}

module Decoder where

import Pixel
import Util

import Data.Bits
import qualified Data.ByteString as BS

data Chunk pixel = One pixel        -- Got a pixel
                 | Repeat Int       -- Pixel should be repeated from previous
                 | Lookback Int     -- Pixel should be taken from the array

decodeChunk :: BS.ByteString -> Int -> Pixel3 -> (Int, Chunk Pixel3)
decodeChunk str pos prevPixel
  -- QOI_OP_RGB
  | byte == 0b11111110 = let r' = str ! pos + 1
                             g' = str ! pos + 2
                             b' = str ! pos + 3
                             (_, _, _, a) = toRGBA prevPixel
                         in (4, One $ fromRGBA r' g' b' a)
  -- QOI_OP_RGBA
  | byte == 0b11111111 = let r' = str ! pos + 1
                             g' = str ! pos + 2
                             b' = str ! pos + 3
                             a' = str ! pos + 4
                         in (5, One $ fromRGBA r' g' b' a')
  | otherwise = case byte .>>. 6 of
      -- QOI_OP_INDEX
      0b00 -> (1, Lookback . fromIntegral $ byte .&. 0b00111111)
      -- QOI_OP_DIFF
      0b01 -> let bias = 2
                  mask = 0b11
                  dr = byte .>>. 4 .&. mask - bias
                  dg = byte .>>. 2 .&. mask - bias
                  db = byte        .&. mask - bias
              in (1, One $ addRGB prevPixel dr dg db)
      -- QOI_OP_LUMA
      0b10 -> let greenBias = 32
                  dg = (byte .&. 0b00111111) - greenBias
                  bias = 8
                  nextByte = str ! pos + 1
                  dr = nextByte .>>. 4 - bias + dg
                  db = nextByte .&. 0b00001111 - bias + dg
              in (2, One $ addRGB prevPixel dr dg db)
      -- QOI_OP_RUN
      0b11 -> (1, Repeat . fromIntegral $ 1 + byte .&. 0b00111111)
      -- impossible case, to make the linter happy
      _ -> error "can't happen"
  where byte = str ! pos
