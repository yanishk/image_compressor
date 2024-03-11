{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Main
-}

module Main (main) where

import GetUserInput
import System.Environment(getArgs)

main :: IO ()
main = do
    userParams <- getArgs
    _ <- checkParamsNb (userParams)
    let finalConfig = getParams (defaultImageCompConfig) (userParams)
    _ <- checkImageCompConfig (finalConfig)
    print finalConfig
