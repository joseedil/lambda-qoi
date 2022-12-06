{-# LANGUAGE Strict #-}

module Data.Codec.Qoi.Util where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS

infix 4 !
(!) :: BS.ByteString -> Int -> Word8
(!) = BS.index

infix 8 .>>., .<<.
(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL
