{-
-- EPITECH PROJECT, 2024
-- bsImageCompressor
-- File description:
-- Cluster
-}

module Cluster (createClusters, recursivePutColor, printClusters) where

import OpenFile
import System.Random
import Data.Maybe

data Cluster = Cluster
    { centroidColor :: Color,
    clusterColors :: Maybe [Color]
    } deriving (Show)

defaultCluster :: Cluster
defaultCluster = Cluster
   { centroidColor = defaultColor,
     clusterColors = Nothing}

chooseRandomColor :: [Color] -> IO Color
chooseRandomColor elemList = do
    index <- randomRIO (0 , (length elemList) - 1)
    return (elemList !! index)

chooseRandomCentroid :: Cluster -> [Color] -> IO Cluster
chooseRandomCentroid cluster colorList = do
    randomColor <- chooseRandomColor colorList
    return cluster {centroidColor = randomColor}

createClusters :: Int -> [Color] -> IO [Cluster]
createClusters 0 _ = return []
createClusters n colorList = do
    cluster <- chooseRandomCentroid (defaultCluster) (colorList)
    rest <- createClusters (n - 1) (colorList)
    return (cluster:rest)

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Double
distance (x1,y1,z1) (x2,y2,z2)
    = sqrt (fromIntegral ((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2))

closest :: Color -> Maybe Double -> Int -> Int -> [Cluster] -> Int
closest _ _ index _ [] = index
closest color Nothing 0 0 (cluster:rest) = closest color dist 0 1 rest
    where
        dist = (Just (distance (fromJust (rgb color))
                (fromJust (rgb (centroidColor cluster)))))
closest color (Just dist1) index iteration (cluster:rest)
    | (Just dist1) > dist2 = closest color dist2 iteration (iteration + 1) rest
    | otherwise = closest color (Just dist1) index iteration rest
    where
        dist2 = (Just (distance (fromJust (rgb color))
                (fromJust (rgb (centroidColor cluster)))))

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

printColor :: Color -> IO ()
printColor color = putStr (show (fromJust (pos color)))
                    >> putStr " " >> print (fromJust (rgb color))

printColorList :: Maybe [Color] -> Int -> IO ()
printColorList (Just []) _ = putStr ""
printColorList Nothing 0 = putStrLn "-"
printColorList colors 0 = putStrLn "-" >> printColorList colors 1
printColorList (Just (color:rest)) n = printColor color >>
                                        printColorList (Just rest) (n + 1)

printCentroid :: Color -> IO ()
printCentroid centroid = putStrLn "--" >>
                        print (fromJust (rgb centroid))

printClusters :: [Cluster] -> IO ()
printClusters [] = putStr ""
printClusters (cluster:r) = printCentroid (centroidColor cluster)
                            >> printColorList (clusterColors cluster) 0
                            >> printClusters r
