{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Cluster
-}

module Cluster
    (Cluster(..),
    createClusters,
    recursivePutColor) where

import OpenFile
import System.Random (randomRIO)
import Data.Maybe (fromJust)

data Cluster = Cluster
    { centroidRgb :: Maybe (Int, Int, Int),
    clusterColors :: Maybe [Color]
    } deriving (Show)

defaultCluster :: Cluster
defaultCluster = Cluster
   { centroidRgb = Nothing,
     clusterColors = Nothing}

chooseRandomColor :: [Color] -> IO Color
chooseRandomColor elemList = do
    index <- randomRIO (0 , length elemList - 1)
    return (elemList !! index)

chooseRandomCentroid :: Cluster -> [Color] -> IO Cluster
chooseRandomCentroid cluster colorList = do
    randomColor <- chooseRandomColor colorList
    return cluster {centroidRgb = rgb randomColor}

createClusters :: Int -> [Color] -> IO [Cluster]
createClusters 0 _ = return []
createClusters n colorList = do
    cluster <- chooseRandomCentroid (defaultCluster) (colorList)
    rest <- createClusters (n - 1) (colorList)
    return (cluster:rest)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (x1,y1,z1) (x2,y2,z2) = sqrt (fromIntegral ((x1 - x2)^(2::Int) +
    (y1 - y2)^(2::Int) + (z1 - z2)^(2::Int)))

closest :: Color -> Maybe Double -> Int -> Int -> [Cluster] -> Int
closest _ _ index _ [] = index
closest color Nothing 0 0 (cluster:rest) = closest color dist 0 1 rest
    where
        dist = (Just (distance (fromJust (rgb color))
            (fromJust (centroidRgb cluster))))
closest color dist1 index iteration (cluster:rest)
    | dist1 > dist2 = closest color dist2 iteration (iteration + 1) rest
    | otherwise = closest color dist1 index iteration rest
    where
        dist2 = (Just (distance (fromJust (rgb color))
            (fromJust (centroidRgb cluster))))

putColor :: Color -> [Cluster] -> [Cluster]
putColor _ [] = []
putColor color clusts =
    take index clusts ++ [newCluster] ++ drop (index + 1) clusts
    where
        index = closest color Nothing 0 0 clusts
        closestClust = (clusts !! index)
        newCluster = case clusterColors closestClust of
            Nothing -> closestClust {clusterColors = Just [color]}
            Just colorList -> closestClust {clusterColors
                = Just (colorList ++ [color])}

recursivePutColor :: [Color] -> [Cluster] -> [Cluster]
recursivePutColor [] clusts = clusts
recursivePutColor (col:r) clusts = recursivePutColor r (putColor col clusts)
