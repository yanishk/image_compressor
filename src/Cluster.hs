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
    clusterPixels :: Maybe [Pixel]
    } deriving (Show)

defaultCluster :: Cluster
defaultCluster = Cluster
   { centroidRgb = Nothing,
     clusterPixels = Nothing}

chooseRandomPixel :: [Pixel] -> IO Pixel
chooseRandomPixel elemList = do
    index <- randomRIO (0 , length elemList - 1)
    return (elemList !! index)

chooseRandomCentroid :: Cluster -> [Pixel] -> IO Cluster
chooseRandomCentroid cluster pixelList = do
    randomPixel <- chooseRandomPixel pixelList
    return cluster {centroidRgb = rgb randomPixel}

createClusters :: Int -> [Pixel] -> IO [Cluster]
createClusters 0 _ = return []
createClusters n pixelList = do
    cluster <- chooseRandomCentroid defaultCluster pixelList
    rest <- createClusters (n - 1) pixelList
    return (cluster:rest)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (x1,y1,z1) (x2,y2,z2) = sqrt (fromIntegral nb)
    where nb = (x1 - x2)^(2::Int) + (y1 - y2)^(2::Int) + (z1 - z2)^(2::Int)

closest :: Pixel -> Maybe Double -> Int -> Int -> [Cluster] -> Int
closest _ _ index _ [] = index
closest pixel Nothing 0 0 (cluster:rest) = closest pixel dist 0 1 rest
    where
        dist = Just (distance (fromJust (rgb pixel))
            (fromJust (centroidRgb cluster)))
closest pixel dist2 index iteration (cluster:rest)
    | dist2 > dist = closest pixel dist iteration (iteration + 1) rest
    | otherwise = closest pixel dist2 index iteration rest
    where
        dist = Just (distance (fromJust (rgb pixel))
            (fromJust (centroidRgb cluster)))

putPixel :: [Cluster] -> Pixel -> [Cluster]
putPixel [] _ = []
putPixel clusts pixel =
    take index clusts ++ [newCluster] ++ drop (index + 1) clusts
    where
        index = closest pixel Nothing 0 0 clusts
        closestClust = clusts !! index
        newCluster = case clusterPixels closestClust of
            Nothing -> closestClust {clusterPixels = Just [pixel]}
            Just pixelList -> closestClust {clusterPixels
                = Just (pixelList ++ [pixel])}

recursivePutPixel :: [Pixel] -> [Cluster] -> [Cluster]
recursivePutPixel [] clusts = clusts
recursivePutPixel pixelList clusts = foldl putPixel clusts pixelList

getPixelsPos :: [Pixel] -> [(Int, Int, Int)]
getPixelsPos [] = []
getPixelsPos pixelList = map (fromJust . rgb) pixelList

meanCentroid :: [Pixel] -> (Int, Int, Int) -> [(Int, Int, Int)] -> (Int, Int, Int)
meanCentroid list (x,y,z) [] = (div x nbElem, div y nbElem, div z nbElem)
    where
        nbElem = length list
meanCentroid list (x1, y1, z1) ((x2,y2,z2):r) = meanCentroid list truple r
    where
        truple = (x1 + x2, y1 + y2, z1 + z2)

newCentroid :: Cluster -> Cluster
newCentroid clust
    | isNothing (clusterPixels clust) = clust
    | otherwise = clust {centroidRgb = Just newCentroidRgb}
        where
            newCentroidRgb = meanCentroid (fromJust (clusterPixels clust))
                (0, 0, 0) (getPixelsPos (fromJust (clusterPixels clust)))

recursiveNewCentroid :: [Cluster] -> [Cluster]
recursiveNewCentroid [] = []
recursiveNewCentroid clusts = map newCentroid clusts

deletePixelList :: Cluster -> Cluster
deletePixelList clust = clust {clusterPixels = Nothing}

recursiveDeleteList :: [Cluster] -> [Cluster]
recursiveDeleteList [] = []
recursiveDeleteList clusts = map deletePixelList clusts

checkLimit :: Cluster -> Cluster -> Double -> Bool
checkLimit clust1 clust2 limit
    | distance (fromJust(centroidRgb clust1))
        (fromJust(centroidRgb clust2)) > limit = False
    | otherwise = True

recursiveCheckLimit :: [Cluster] -> [Cluster] -> Double -> Bool -> Bool
recursiveCheckLimit _ _ _ True = True
recursiveCheckLimit [] _  _ False = False
recursiveCheckLimit _ [] _ False = False
recursiveCheckLimit (clusts1:rest1) (clusts2:rest2) limit False =
    recursiveCheckLimit rest1 rest2 limit (checkLimit clusts1 clusts2 limit)

iterateTillLimit :: [Pixel] -> [Cluster] -> Double -> Maybe Bool -> [Cluster]
iterateTillLimit _ clusts _ (Just True) = clusts
iterateTillLimit pixelL clusts limit Nothing =
    iterateTillLimit pixelL newClusts limit boolRes
    where
        newClusts = recursivePutPixel pixelL clusts
        boolRes = Just False
iterateTillLimit pixelL clusts limit (Just False) =
    iterateTillLimit pixelL newClusts limit boolRes
    where
        newClusts = recursivePutPixel pixelL resetClusts
        resetClusts = recursiveDeleteList (recursiveNewCentroid clusts)
        boolRes = Just (recursiveCheckLimit clusts clusts limit False)
