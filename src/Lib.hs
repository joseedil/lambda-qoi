{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -O2 -fllvm #-}

module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
