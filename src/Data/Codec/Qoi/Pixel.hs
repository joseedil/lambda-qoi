{-# LANGUAGE Strict #-}

module Data.Codec.Qoi.Pixel where

import Data.Word (Word8)
import Codec.Picture
import qualified Data.Vector as V


type Pixel3 = PixelRGB8
type Pixel4 = PixelRGBA8

data DynamicPixels = Pixels3 (V.Vector Pixel3)
                   | Pixels4 (V.Vector Pixel4)


class PixelDecode a where
  toRGBA :: a -> (Word8, Word8, Word8, Word8)
  fromRGBA :: Word8 -> Word8 -> Word8 -> Word8 -> a
  channelCount :: a -> Int
  diffPixel :: a -> a -> (Word8, Word8, Word8, Word8)

instance PixelDecode PixelRGB8 where
  toRGBA (PixelRGB8 r g b) = (r, g, b, 255)
  fromRGBA r g b _ = PixelRGB8 r g b
  channelCount _ = 3
  diffPixel p0 p1 = (r1 - r0, g1 - g0, b1 - b0, a1 - a0)
    where (r1, g1, b1, a1) = toRGBA p1
          (r0, g0, b0, a0) = toRGBA p0


instance PixelDecode PixelRGBA8 where
  toRGBA (PixelRGBA8 r g b a) = (r, g, b, a)
  fromRGBA = PixelRGBA8
  channelCount _ = 4
  diffPixel p0 p1 = (r1 - r0, g1 - g0, b1 - b0, a1 - a0)
    where (r1, g1, b1, a1) = toRGBA p1
          (r0, g0, b0, a0) = toRGBA p0

addRGB :: PixelDecode p => p -> Word8 -> Word8 -> Word8 -> p
addRGB px dr dg db = addRGBA px dr dg db 0

addRGBA :: PixelDecode p => p -> Word8 -> Word8 -> Word8 -> Word8 -> p
addRGBA px dr dg db da = let (r, g, b, a) = toRGBA px
                         in fromRGBA (r + dr) (g + dg) (b + db) (a + da)

pixelHash :: (Num a, PixelDecode p) => p -> a
pixelHash px = fromIntegral $ (r * 3 + g * 5 + b * 7 + a * 11) `mod` 64
  where (r, g, b, a) = toRGBA px

initialPixel :: PixelDecode px => px
initialPixel = fromRGBA 0 0 0 255
