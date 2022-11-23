{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Pixel where

import Data.Word (Word8)
import qualified Data.ByteString as BS

import Util

data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show)
data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show)

addPixel3 :: Pixel3 -> Pixel3 -> Pixel3
addPixel3 (Pixel3 r1 g1 b1) (Pixel3 r2 g2 b2) = Pixel3 (r1 + r2) (g1 + g2) (b1 + b2)

addRGB :: Pixel p => p -> Word8 -> Word8 -> Word8 -> p
addRGB px dr dg db = addRGBA px dr dg db 0

addRGBA :: Pixel p => p -> Word8 -> Word8 -> Word8 -> Word8 -> p
addRGBA px dr dg db da = let (r, g, b, a) = toRGBA px
                         in fromRGBA (r + dr) (g + dg) (b + db) (a + da)

pixelHash :: (Num a, Pixel p) => p -> a
pixelHash px = fromIntegral $ (r * 3 + g * 5 + b * 7 + a * 11) `mod` 64
  where (r, g, b, a) = toRGBA px

class Pixel a where
  toRGBA :: a -> (Word8, Word8, Word8, Word8)
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a

  readPixel :: BS.ByteString -> Int -> a
  channelCount :: proxy a -> Int

  initialPixel :: a

instance Pixel Pixel3 where
  toRGBA (Pixel3 r g b) = (r, g, b, 255)
  fromRGBA r g b _ = Pixel3 r g b
  readPixel str pos = Pixel3 (str ! pos) (str ! pos + 1) (str ! pos + 2)
  channelCount _ = 3
  initialPixel = fromRGBA 0 0 0 255

instance Pixel Pixel4 where
  toRGBA (Pixel4 r g b a) = (r, g, b, a)
  fromRGBA = Pixel4
  readPixel str pos = Pixel4 (str ! pos) (str ! pos + 1) (str ! pos + 2) (str ! pos + 3)
  channelCount _ = 4
  initialPixel = fromRGBA 0 0 0 255
