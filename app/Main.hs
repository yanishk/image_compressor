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
import System.Exit (exitWith, ExitCode(ExitFailure))
import System.Directory(doesFileExist)

removeMaybeInt :: Maybe Int -> [Int]
removeMaybeInt Nothing = []
removeMaybeInt (Just x) = [x]

maybereadFile :: String -> IO String
maybereadFile path = do
    exists <- doesFileExist path
    case exists of
        True -> readFile path
        False -> exitWith (ExitFailure 84)

finalReadFile :: ImageCompConfig -> IO (String)
finalReadFile col =
    maybereadFile (notMaybeFilepath)
        where notMaybeFilepath = fromJust (filePath col)

main :: IO ()
main = do
    userParams <- getArgs
    _ <- checkParamsNb userParams
    let midConfig = getParams defaultImageCompConfig userParams
    _ <- checkImageCompConfig midConfig
    let finalConfig = fromJust midConfig
    fileOpen <- finalReadFile finalConfig
    finalClusters <- processFile finalConfig fileOpen
    printClusters finalClusters

processFile :: ImageCompConfig -> String -> IO [Cluster]
processFile finalConfig fileOpen = do
    let colorList = parseFiletoStruct (lines fileOpen)
    let nbColor = nbOfColors finalConfig
    defCluster <- createClusters (head (removeMaybeInt nbColor)) colorList
    return (iterateTillLimit colorList defCluster 0)
