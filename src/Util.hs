
module Util where

import Data.Int (Int64)
import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as BS

infix 4 !
(!) :: BS.ByteString -> Int64 -> Word8
(!) = BS.index

infix 8 .>>., .<<.
(.>>.), (.<<.) :: Bits a => a -> Int -> a
(.>>.) = shiftR
(.<<.) = shiftL
