module Main (main) where

import Decoder

import System.Environment
import System.Exit
import Codec.Picture
import qualified Data.ByteString as BS

-- main :: IO ()
-- main = do
--   args <- getArgs
--   if null args
--     then do putStrLn "usage: lambda-qoi filename"
--             exitWith $ ExitFailure 1
--     else do
--     file <- BS.readFile $ head args
--     result <- return . decodeQoi $ file
--     print result
--     exitSuccess


main :: IO ()
main = do
  args <- getArgs
  case length args of
    2 -> do
      let qoiFilename = head args
          pngFilename = args !! 1
      file <- BS.readFile qoiFilename
      let result = decodeQoiPng file
      case result of
        Nothing -> do putStrLn "Decoding failed"
                      exitWith $ ExitFailure 2
        Just image -> do putStrLn "Saving image"
                         writePng pngFilename image
                         exitSuccess
    _ -> do putStrLn "usage: lambda-qoi qoi-filename png-filename"
            exitWith $ ExitFailure 1
