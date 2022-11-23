module Main (main) where

import Decoder

import System.Environment
import System.Exit
import qualified Data.ByteString as BS

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do putStrLn "usage: lambda-qoi filename"
            exitWith $ ExitFailure 1
    else do
    file <- BS.readFile $ head args
    result <- return . decodeQoi $ file
    print result
    exitSuccess
