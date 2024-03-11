{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- GetUserInput
-}

module GetUserInput
    ( defaultImageCompConfig,
      checkParamsNb,
      getParams,
      checkImageCompConfig,
    ) where

import Text.Read(readMaybe)
import System.Exit

data ImageCompConfig = ImageCompConfig
    {   nbOfColors :: Maybe Int,
        convergenceLimit :: Maybe Double,
        filePath :: Maybe String } deriving (Show, Eq)

defaultImageCompConfig :: ImageCompConfig
defaultImageCompConfig = ImageCompConfig
    { nbOfColors = Nothing,
      convergenceLimit = Nothing,
      filePath = Nothing }

checkParamsNb :: [String] -> IO (Maybe Int)
checkParamsNb params
    | length params == 6 = return (Just 0)
    | otherwise = exitWith (ExitFailure 84)

getParams :: ImageCompConfig -> [String] -> Maybe ImageCompConfig
getParams conf ("-n":nb:r) = getParams conf {nbOfColors = readMaybe nb} r
getParams conf ("-l":nb:r) = getParams conf {convergenceLimit = readMaybe nb} r
getParams conf ("-f":file:r) = getParams conf {filePath = (Just file)} r
getParams conf [] = Just conf
getParams _ _ = Nothing

checkImageCompConfig :: Maybe ImageCompConfig -> IO (Maybe Int)
checkImageCompConfig Nothing = exitWith (ExitFailure 84)
checkImageCompConfig (Just config)
    | (nbOfColors config) == Nothing = exitWith (ExitFailure 84)
    | (nbOfColors config) < (Just 0) = exitWith (ExitFailure 84)
    | (convergenceLimit config) == Nothing = exitWith (ExitFailure 84)
    | (convergenceLimit config) < (Just 0) ||
    (convergenceLimit config) > (Just 1) = exitWith (ExitFailure 84)
    | (filePath config) == Nothing = exitWith (ExitFailure 84)
    | otherwise = return (Just 0)
