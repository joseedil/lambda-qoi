{-# LANGUAGE Strict #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Decoder where

import Header
import Pixel
import Util

import Data.Bits
import Data.Binary (decodeOrFail)
import Codec.Picture

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM



data Chunk pixel = One pixel        -- Got a pixel
                 | Repeat Int       -- Pixel should be repeated from previous
                 | Lookback Int     -- Pixel should be taken from the array

decodeChunk :: Pixel.Pixel px => BS.ByteString -> Int -> px -> (Int, Chunk px)
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

decodeQoi :: Pixel.Pixel px => BS.ByteString -> Int -> V.Vector px
decodeQoi str n = V.create $ do
  -- dataVec is where we store our data during the stateful computation
  dataVec <- VM.new n

  -- running is the running array that is updated every time we
  -- discover a new pixel
  running <- VM.replicate 64 $ fromRGBA 0 0 0 0

  -- update function to the running vector
  let updateRunning px = VM.write running (pixelHash px) px

  -- The working loop
  -- inPos manages the consumption of the input string
  -- outPos manages the writing on the data vector
  -- prevPixel is the last pixel we have seem
  let loop inPos outPos prevPixel
        -- do not go further than the size of the data vector
        | outPos < n = do
            -- decode next chunk
            let (diff, chunk) = decodeChunk str inPos prevPixel
            case chunk of
              One px -> do VM.write dataVec outPos px
                           updateRunning px
                           loop (inPos + diff) (outPos + 1) px
              Lookback pos -> do px <- VM.read running pos
                                 VM.write dataVec outPos px
                                 loop (inPos + diff) (outPos + 1) px
              Repeat qty -> do VM.set (VM.slice outPos qty dataVec) prevPixel
                               loop (inPos + diff) (outPos + qty) prevPixel
        | otherwise = pure ()

  -- This will actually run the computation
  loop 0 0 initialPixel

  -- Return dataVec to freeze the final Vector
  return dataVec

data DynamicPixels = Pixels3 (V.Vector Pixel3)
                   | Pixels4 (V.Vector Pixel4)

decodeQoiBS :: BS.ByteString -> Maybe (Header, DynamicPixels)
decodeQoiBS str = case decodeOrFail . BSL.fromStrict $ BS.take 14 str of
  Left _ -> Nothing
  Right (_, _, h@Header {..}) ->
    if hChannels == RGB
    then Just (h, Pixels3 $ decodeQoi (BS.drop 14 str) (fromIntegral $ hWidth * hHeight))
    else Just (h, Pixels4 $ decodeQoi (BS.drop 14 str) (fromIntegral $ hWidth * hHeight))


decodeQoiPng :: BS.ByteString -> Maybe (Header, Image PixelRGBA8)
decodeQoiPng str =
  case decodeOrFail . BSL.fromStrict $ BS.take 14 str of
    Left _ -> Nothing
    Right (_, _, header@Header {..}) ->
      case hChannels of
        RGB -> Just (header, generateImage (vectorToImage decoded3ch) w h)
        RGBA -> Just (header, generateImage (vectorToImage decoded4ch) w h)
      where
        w = fromIntegral hWidth
        h = fromIntegral hHeight

        decoded3ch = Pixels3 $ decodeQoi (BS.drop 14 str) (fromIntegral $ hWidth * hHeight)
        decoded4ch = Pixels4 $ decodeQoi (BS.drop 14 str) (fromIntegral $ hWidth * hHeight)

        vectorToImage :: DynamicPixels -> Int -> Int -> PixelRGBA8
        vectorToImage dyn x y =
          case dyn of
            Pixels3 vec -> let (r, g, b, a) = toRGBA $ vec V.! i
                           in PixelRGBA8 r g b a
            Pixels4 vec -> let (r, g, b, a) = toRGBA $ vec V.! i
                           in PixelRGBA8 r g b a
          where
            i = y * w + x
