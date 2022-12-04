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
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM



type Encoder s = VM.STVector s Word8 -> Int -> Maybe (ST s Int)

-- Checks if -bound <= delta < bound
isBounded :: Word8 -> Word8 -> Bool
isBounded delta bound = delta + bound < 2 * bound


--QOI_OP_DIFF
encodeDiff :: Word8 -> Word8 -> Word8 -> Encoder s
encodeDiff dr dg db out outPos
  | all (isBounded 2) [dr, dg, db] = let byte = 0b01000000 .|. ((dr + 2) .<<. 4)
                                                           .|. ((dg + 2) .<<. 2)
                                                           .|. (db + 2)
                                     in Just $ VM.write out outPos byte $> 1
  | otherwise = Nothing


-- QOI_OP_LUMA
encodeLuma :: Word8 -> Word8 -> Word8 -> Word8 -> Encoder s
encodeLuma dr dg db da out outPos
  | da == 0
  , isBounded dg 32
  , all (isBounded 8) [dr - dg, db - dg] = let b1 = 0b10000000 .|. (dg + 32)
                                               b2 = (dr - dg + 8) .<<. 4
                                                 .|. (db - dg + 8)
                                           in Just $ do VM.write out outPos b1
                                                        VM.write out (outPos+ 1) b2
                                                        return 2
  | otherwise = Nothing


-- QOI_OP_INDEX
encodeIndex :: Eq px => px -> Word8 -> px -> Encoder s
encodeIndex px hash runningPx out outPos
  | px == runningPx = Just $ VM.write out outPos hash $> 1
  | otherwise = Nothing


-- QOI_OP_RGB and QOI_OP_RGBA
encodeRGB :: PixelDecode px => px -> px -> Encoder s
encodeRGB px prevPixel out outPos = Just $ do
  let (r, g, b, a) = toRGBA px
      (_, _, _, prevAlpha) = toRGBA prevPixel
      da = fromEnum $ a /= prevAlpha
  VM.write out outPos $ 0b11111110 .|. fromIntegral da
  VM.write out (outPos + 1) r
  VM.write out (outPos + 2) g
  VM.write out (outPos + 3) b
  when (da == 1) $ VM.write out (outPos + 4) a
  return (4 + da)


-- QOI_OP_RUN
encodeRun :: Int -> VM.STVector s Word8 -> Int -> ST s Int
encodeRun 0 _ outPos = return outPos
encodeRun runLen out outPos = VM.write out outPos byte $> outPos + 1
  where byte = 0b11000000 .|. fromIntegral (runLen - 1)
