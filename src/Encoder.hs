{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}

module Encoder where

import Header
import Pixel
import Util

import Data.Bits
import Data.Word (Word8)
import Data.Functor
import Data.Binary (decodeOrFail)
import Control.Monad (when)
import Control.Monad.ST

import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM


-- Checks if -bound <= delta < bound
isBounded :: Word8 -> Word8 -> Bool
isBounded delta bound = delta + bound < 2 * bound


--QOI_OP_DIFF
encodeDiff :: Word8 -> Word8 -> Word8 -> Maybe BSB.Builder
encodeDiff dr dg db
  | all (isBounded 2) [dr, dg, db] =
      let byte = 0b01000000 .|. ((dr + 2) .<<. 4)
                            .|. ((dg + 2) .<<. 2)
                            .|. (db + 2)
      in Just $ BSB.word8 byte
  | otherwise = Nothing


-- QOI_OP_LUMA
encodeLuma :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe BSB.Builder
encodeLuma dr dg db da
  | da == 0
  , isBounded dg 32
  , all (isBounded 8) [dr - dg, db - dg] =
    let b1 = 0b10000000 .|. (dg + 32)
        b2 = (dr - dg + 8) .<<. 4
         .|. (db - dg + 8)
    in Just $ BSB.word8 b1 <> BSB.word8 b2
  | otherwise = Nothing


-- QOI_OP_INDEX
encodeIndex :: Eq px => px -> Word8 -> px -> Maybe BSB.Builder
encodeIndex px hash runningPx
  | px == runningPx = Just $ BSB.word8 hash
  | otherwise = Nothing


-- QOI_OP_RGB and QOI_OP_RGBA
encodeRGB :: PixelDecode px => px -> px -> Maybe BSB.Builder
encodeRGB px prevPixel =
  let (r, g, b, a) = toRGBA px
      (_, _, _, prevAlpha) = toRGBA prevPixel
      da = fromEnum $ a /= prevAlpha
  in Just $ mconcat
     [
       BSB.word8 (0b11111110 .|. fromIntegral da),
       BSB.word8 r,
       BSB.word8 g,
       BSB.word8 b,
       if da == 1 then BSB.word8 a else mempty
     ]


-- QOI_OP_RUN
encodeRun :: Int -> BSB.Builder
encodeRun 0 = mempty
encodeRun runLen = BSB.word8 $ 0b11000000 .|. fromIntegral (runLen - 1)
