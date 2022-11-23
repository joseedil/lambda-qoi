{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Header where

import Data.Binary
import Data.Binary.Get (getWord32be)
import GHC.Generics (Generic)


data Channels = RGB | RGBA deriving (Eq, Show, Generic)

instance Binary Channels where
  put RGB  = putWord8 3
  put RGBA = putWord8 4

  get = do c <- getWord8
           case c of
             3 -> return RGB
             4 -> return RGBA
             _ -> fail "Invalid channel byte in header"


data Colorspace = SRGBAlpha | Linear deriving (Eq, Show, Generic)

instance Binary Colorspace where
  put SRGBAlpha = putWord8 0
  put Linear    = putWord8 1

  get = do c <- getWord8
           case c of
             0 -> return SRGBAlpha
             1 -> return Linear
             _ -> fail "Invalid colorspace byte in header"


data Header = Header
  { -- There is a magic string in the beginning, but we treat it in the serialization functions
    hWidth :: Word32,
    hHeight :: Word32,
    hChannels :: Channels,
    hColorspace :: Colorspace
  } deriving (Eq, Show, Generic)

instance Binary Header where
  put (Header width height channels colorspace) = do
    putList magic
    put width
    put height
    put channels
    put colorspace
      where magic :: [Word8]
            magic = [113, 111, 105, 102]

  get = do
   magicWord <- get :: Get (Word8, Word8, Word8, Word8)
   if validMagic magicWord
     then Header <$> getWord32be <*> getWord32be <*> (get :: Get Channels) <*> (get :: Get Colorspace)
     else fail "Invalid magic word"
     where validMagic :: (Word8, Word8, Word8, Word8) -> Bool
           validMagic word = word == (113, 111, 105, 102)
