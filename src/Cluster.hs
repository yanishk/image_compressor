{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- Cluster
-}

module Cluster
    (Cluster(..),
    createClusters,
    iterateTillLimit) where

import OpenFile
import System.Random(randomRIO)
import Data.Maybe(fromJust, isNothing)

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
    cluster <- chooseRandomCentroid defaultCluster colorList
    rest <- createClusters (n - 1) colorList
    return (cluster:rest)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (x1,y1,z1) (x2,y2,z2) = sqrt (fromIntegral nb)
    where nb = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) + (z1 - z2)^(2::Int)

closest :: Color -> Maybe Double -> Int -> Int -> [Cluster] -> Int
closest _ _ index _ [] = index
closest color Nothing 0 0 (cluster:rest) = closest color dist 0 1 rest
    where
        dist = Just (distance (fromJust (rgb color))
            (fromJust (centroidRgb cluster)))
closest color dist2 index iteration (cluster:rest)
    | dist2 > dist = closest color dist iteration (iteration + 1) rest
    | otherwise = closest color dist2 index iteration rest
    where
        dist = Just (distance (fromJust (rgb color))
            (fromJust (centroidRgb cluster)))

putColor :: [Cluster] -> Color -> [Cluster]
putColor [] _ = []
putColor clusts color =
    take index clusts ++ [newCluster] ++ drop (index + 1) clusts
    where
        index = closest color Nothing 0 0 clusts
        closestClust = clusts !! index
        newCluster = case clusterColors closestClust of
            Nothing -> closestClust {clusterColors = Just [color]}
            Just colorList -> closestClust {clusterColors
                = Just (colorList ++ [color])}

recursivePutColor :: [Color] -> [Cluster] -> [Cluster]
recursivePutColor [] clusts = clusts
recursivePutColor colorList clusts = foldl putColor clusts colorList

getColorsPos :: [Color] -> [(Int, Int, Int)]
getColorsPos [] = []
getColorsPos colorList = map (fromJust . rgb) colorList

meanCentroid :: [Color] -> (Int, Int, Int) -> [(Int, Int, Int)] -> (Int, Int, Int)
meanCentroid list (x,y,z) [] = (div x nbElem, div y nbElem, div z nbElem)
    where
        nbElem = length list
meanCentroid list (x1, y1, z1) ((x2,y2,z2):r) = meanCentroid list truple r
    where
        truple = (x1 + x2, y1 + y2, z1 + z2)

newCentroid :: Cluster -> Cluster
newCentroid clust
    | isNothing (clusterColors clust) = clust
    | otherwise = clust {centroidRgb = Just newCentroidRgb}
        where
            newCentroidRgb = meanCentroid (fromJust (clusterColors clust))
                (0, 0, 0) (getColorsPos (fromJust (clusterColors clust)))

recursiveNewCentroid :: [Cluster] -> [Cluster]
recursiveNewCentroid [] = []
recursiveNewCentroid clusts = map newCentroid clusts

deleteColorList :: Cluster -> Cluster
deleteColorList clust = clust {clusterColors = Nothing}

recursiveDeleteList :: [Cluster] -> [Cluster]
recursiveDeleteList [] = []
recursiveDeleteList clusts = map deleteColorList clusts

iterateTillLimit :: [Color] -> [Cluster] -> Int -> [Cluster]
iterateTillLimit _ clusts 15 = clusts
iterateTillLimit colorL clusts 0 = iterateTillLimit colorL newClusts 1
    where
        newClusts = recursivePutColor colorL clusts
iterateTillLimit colorL clusts n =  iterateTillLimit colorL newClusts (n + 1)
    where
        newClusts = recursivePutColor colorL resetClusts
        resetClusts = recursiveDeleteList (recursiveNewCentroid clusts)
