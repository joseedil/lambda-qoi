{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecordWildCards #-}

module Decoder where

import Header
import Pixel
import Util

import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Binary (decodeOrFail)

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

decode3ch :: BS.ByteString -> Int -> V.Vector Pixel3
decode3ch str n = V.create $ do
  -- dataVec is where we store our data during the stateful computation
  dataVec <- VM.new n

  -- running is the running array that is updated every time we
  -- discover a new pixel
  running <- VM.replicate 64 $ fromRGBA 0 0 0 255

  -- update function to the running vector
  let updateRunning px = VM.write running (pixelHash px) px

  -- The working loop
  -- inPos manages the consumption of the input string
  -- outPos manages the writing on the data vector
  -- prevPixel is the last pixel we have seem
  let step inPos outPos prevPixel
        -- do not go further than the size of the data vector
        | outPos < n = do
            -- decode next chunk
            let (diff, chunk) = decodeChunk str inPos prevPixel
            case chunk of
              One px -> do VM.write dataVec outPos px
                           updateRunning px
                           step (inPos + diff) (outPos + 1) px
              Lookback pos -> do px <- VM.read running pos
                                 VM.write dataVec outPos px
                                 step (inPos + diff) (outPos + 1) px
              Repeat qty -> do VM.set (VM.slice outPos qty dataVec) prevPixel
                               step (inPos + diff) (outPos + qty) prevPixel
        | otherwise = pure ()

  -- This will actually run the computation
  step 0 0 (Pixel3 0 0 0)

  -- Return dataVec to freeze the final Vector
  return dataVec


decodeQoi :: BS.ByteString -> Maybe (Header, V.Vector Pixel3)
decodeQoi str = case decodeOrFail . BSL.fromStrict $ BS.take 14 str of
  Left _ -> Nothing
  Right (_, _, h@Header {..}) -> Just (h, decode3ch (BS.drop 14 str) (fromIntegral $ hWidth * hHeight))
