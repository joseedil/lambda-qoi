{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Pixel where

import Data.Word (Word8)


data Pixel3 = Pixel3 Word8 Word8 Word8 deriving (Show)

data Pixel4 = Pixel4 Word8 Word8 Word8 Word8 deriving (Show) 

