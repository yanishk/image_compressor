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
import System.Environment(getArgs)
import Data.Maybe
import System.Exit (exitWith, ExitCode(..))
import Text.Read

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
    let finalClusters = recursivePutColor colorList defCluster
    printClusters finalClusters
