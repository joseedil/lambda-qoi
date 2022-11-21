module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Binary
import Data.Binary.Get

import qualified Data.ByteString.Lazy as B

import Header

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Header"
  [
    decodeValidHeader,
    decodeInvalidMagic,
    decodeInvalidChannels,
    decodeInvalidColorspace
  ]

decodeValidHeader :: TestTree
decodeValidHeader = testCase "decodeValidHeader" $
  decode (B.pack [113, 111, 105, 102, 0, 0, 0, 10, 0, 0, 0, 10, 3, 0]) @?=
  Header {hWidth = 10, hHeight = 10, hChannels = RGB, hColorspace = SRGBAlpha}

decodeInvalidMagic :: TestTree
decodeInvalidMagic = testCase "decodeInvalidMagic" $
  (decodeOrFail (B.pack [113, 112, 105, 102, 0, 0, 0, 10, 0, 0, 0, 10, 3, 0]) :: Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, Header))
  @?= Left ("\NUL\NUL\NUL\n\NUL\NUL\NUL\n\ETX\NUL", 4, "Invalid magic word")

decodeInvalidChannels :: TestTree
decodeInvalidChannels = testCase "decodeInvalidChannels" $
  (decodeOrFail (B.pack [113, 111, 105, 102, 0, 0, 0, 10, 0, 0, 0, 10, 1, 0]) :: Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, Header))
  @?= Left ("\NUL", 13, "Invalid channel byte in header")

decodeInvalidColorspace :: TestTree
decodeInvalidColorspace = testCase "decodeInvalidColorspace" $
  (decodeOrFail (B.pack [113, 111, 105, 102, 0, 0, 0, 10, 0, 0, 0, 10, 3, 2]) :: Either (B.ByteString, ByteOffset, String) (B.ByteString, ByteOffset, Header))
  @?= Left ("", 14, "Invalid colorspace byte in header")

