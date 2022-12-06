module Main (main) where

import Decoder
import Encoder

import System.Environment
import System.Exit
import Codec.Picture
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  args <- getArgs
  case length args of
    3 -> do
      let flag = head args
          inputFilename = args !! 1
          outputFilename = args !! 2
      case flag of
        "d" -> do
          file <- BS.readFile inputFilename
          let result = decodeQoiPng file
          case result of
            Nothing -> do putStrLn "Decoding failed"
                          exitWith $ ExitFailure 2
            Just (_, image) -> do writePng outputFilename image
                                  exitSuccess

        "e" -> do
          file <- BS.readFile inputFilename
          case decodePng file of
            Left err -> do putStrLn "Failed decoding PNG file"
                           putStrLn err
                           exitWith $ ExitFailure 3
            Right dynImage ->
              case dynImage of
                ImageRGB8 image -> do
                  BSL.writeFile outputFilename $ encodeImageRGB image
                  exitSuccess
                ImageRGBA8 image -> do
                  BSL.writeFile outputFilename $ encodeImageRGBA image
                  exitSuccess
                _ -> do putStrLn "Don't know how to encode this kind of image"
                        exitWith $ ExitFailure 4

        _ -> do putStrLn "usage: flag must be 'e'ncode or 'd'ecode"
                exitWith $ ExitFailure 1
    _ -> do putStrLn "usage: lambda-qoi [e/d] input-filename output-filename"
            exitWith $ ExitFailure 1
