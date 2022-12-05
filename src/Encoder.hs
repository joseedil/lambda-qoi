{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Encoder where

import Header
import Pixel
import Util

import Data.Bits
import Data.Word (Word8)
--import Data.Functor
--import Data.Binary (encode)
--import Control.Monad (when)
import Control.Monad.ST

--import Codec.Picture

--import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
--import qualified Data.ByteString.Lazy as BSL

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import Data.Data
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.STRef
import Codec.Picture
import Data.Binary (encode)


-- Checks if -bound <= delta < bound
isBounded :: Word8 -> Word8 -> Bool
isBounded delta bound = delta + bound < 2 * bound


--QOI_OP_DIFF
encodeDiff :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe BSB.Builder
encodeDiff dr dg db da
  | da == 0
  , all (isBounded 2) [dr, dg, db] =
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

maxRunLen :: Int
maxRunLen = 62

maxResultSize :: Header -> Int
maxResultSize Header {..} = fromIntegral $ hWidth * hHeight

-- Encoder worker
encodeData :: forall pixel. (PixelDecode pixel, Eq pixel) => Proxy pixel -> V.Vector pixel -> BSB.Builder
encodeData proxy pixelVect = V.foldl' (<>) mempty $ V.create $ do
  running <- VM.replicate 64 $ fromRGBA @pixel 0 0 0 0

  builders <- VM.new (V.length pixelVect)

  buildersPos <- newSTRef @Int 0

  let --loop :: forall s. BSB.Builder -> Int -> Int -> pixel -> ST s BSB.Builder
      loop inPos runLen prevPixel outPos
        | inPos <= inLen
        , actualPixel == prevPixel =
            if runLen /= maxRunLen - 1
            then loop (inPos + 1) (runLen + 1) prevPixel outPos
            else do VM.write builders outPos $ encodeRun maxRunLen
                    writeSTRef buildersPos outPos
                    loop (inPos + 1) 0 prevPixel (outPos + 1)
        | inPos <= inLen = do
            let
                (r1, g1, b1, a1) = toRGBA actualPixel
                (r0, g0, b0, a0) = toRGBA prevPixel
                (dr, dg, db, da) = (r1 - r0, g1 - g0, b1 - b0, a1 - a0)
                hash = pixelHash actualPixel
            VM.write builders outPos $ encodeRun runLen
            writeSTRef buildersPos outPos
            case encodeDiff dr dg db da of
              Just result -> do VM.write running hash actualPixel
                                VM.write builders (outPos + 1) result
                                writeSTRef buildersPos (outPos + 1)
                                loop (inPos + 1) 0 actualPixel (outPos + 2)
              _ -> do runningPixel <- VM.read running hash
                      VM.write builders (outPos + 1) $
                        fromJust $ encodeIndex actualPixel (fromIntegral hash) runningPixel
                                <|> encodeLuma dr dg db da
                                <|> encodeRGB actualPixel prevPixel
                      writeSTRef buildersPos (outPos + 1)
                      loop (inPos + 1) 0 actualPixel (outPos + 2)
        | otherwise = VM.write builders outPos $ encodeRun runLen
        where
          inLen = V.length pixelVect
          actualPixel = pixelVect V.! inPos

  loop 0 0 (fromRGBA 0 0 0 255) 0
  l <- readSTRef buildersPos
  return $ VM.slice 0 l builders

-- TODO: convert image to vector of pixels

encodeImage :: PixelDecode pixel => Image pixel -> BSB.Builder
encodeImage image@Image{..} =
  let header = Header { hWidth = fromIntegral imageWidth,
                        hHeight = fromIntegral imageHeight,
                        hChannels = RGB,
                        hColorspace = SRGBAlpha }
      headStream = BSB.lazyByteString . encode $ header
      endStream = mconcat (replicate 7 $ BSB.word8 0) <> BSB.word8 1

      if

      imgData = VS.convert imageData
      qoiData = encodeData @Pixel3 Proxy imgData

  in mconcat
     [
       headStream,
       qoiData,
       endStream
     ]
