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
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Exception
import System.IO.Error
import Data.Maybe (fromJust)

removeMaybeInt :: Maybe Int -> [Int]
removeMaybeInt Nothing = []
removeMaybeInt (Just x) = [x]

maybereadFile :: String -> IO String
maybereadFile path = readFile path `catch` handlingFailRead
    where
        handlingFailRead :: IOError -> IO String
        handlingFailRead errorFile
            | isDoesNotExistError errorFile = exitWith (ExitFailure 84)
            | isPermissionError errorFile = exitWith (ExitFailure 84)
            | otherwise = readFile path

finalReadFile :: ImageCompConfig -> IO (String)
finalReadFile col =
    maybereadFile (notMaybeFilepath)
        where notMaybeFilepath = fromJust (filePath col)

compStructFile :: [Pixel] -> [String] -> Bool
compStructFile listPixel file
    | length listPixel == length file = True
    | otherwise = False

checkLetter :: Bool -> IO()
checkLetter True = return ()
checkLetter False = exitWith (ExitFailure 84)

checkIsAShort :: Bool -> [Pixel] -> [Cluster] -> Maybe Double -> IO [Cluster]
checkIsAShort True pixelList defCluster limit =
    return (iterateTillLimit pixelList defCluster
        Nothing (fromJust limit) Nothing)
checkIsAShort False _ _ _ = exitWith (ExitFailure 84)


processFile :: ImageCompConfig -> String -> IO [Cluster]
processFile finalConfig fileOpen = do
    let pixelList = parseFiletoStruct (lines fileOpen)
    checkLetter (compStructFile pixelList (lines fileOpen))
    let nbClusters = nbOfColors finalConfig
    let limit = convergenceLimit finalConfig
    defCluster <- createClusters (head (removeMaybeInt nbClusters)) pixelList
    return (iterateTillLimit pixelList defCluster
        Nothing (fromJust limit) Nothing)
    checkIsAShort (checkAllTuples pixelList) pixelList defCluster limit

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
