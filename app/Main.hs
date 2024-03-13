{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Main
-}

module Main (main) where

import GetUserInput
import OpenFile
import System.Environment(getArgs)
import Data.Maybe

main :: IO ()
main = do
    userParams <- getArgs
    _ <- checkParamsNb (userParams)
    let midConfig = getParams (defaultImageCompConfig) (userParams)
    _ <- checkImageCompConfig (midConfig)
    let finalConfig = fromJust midConfig
    let notMaybeFilepath = fromJust (filePath finalConfig)
    fileOpen <- readFile (notMaybeFilepath)
    let colorList = parseFiletoStruct (lines fileOpen)
    --defClusters <- createClusters 2 colorList
    print colorList
