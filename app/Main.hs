{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Main
-}

module Main (main) where

import GetUserInput
import OpenFile
import Cluster
import PrintClusters
import System.Environment(getArgs)
import Data.Maybe

removeMaybeInt :: Maybe Int -> [Int]
removeMaybeInt Nothing = []
removeMaybeInt (Just x) = [x]

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
    let nbColor = (nbOfColors finalConfig)
    defCluster <- createClusters (head (removeMaybeInt nbColor)) colorList
    let finalClusters = iterateTillLimit colorList defCluster 0
    printClusters finalClusters
