{-
-- EPITECH PROJECT, 2024
-- imageCompressor
-- File description:
-- PrintClusters
-}

module PrintClusters (printClusters) where

import OpenFile
import Cluster
import Data.Maybe(fromJust)

printColor :: Color -> IO ()
printColor color = putStr (show (fromJust (pos color)))
                    >> putStr " " >> print (fromJust (rgb color))

printColorList :: Maybe [Color] -> Int -> IO ()
printColorList (Just []) _ = putStr ""
printColorList Nothing _ = putStrLn "-"
printColorList colors 0 = putStrLn "-" >> printColorList colors 1
printColorList (Just (color:rest)) n = printColor color >>
                                        printColorList (Just rest) (n + 1)

printCentroid :: (Int, Int, Int) -> IO ()
printCentroid centroid = putStrLn "--" >> print centroid

printClusters :: [Cluster] -> IO ()
printClusters [] = putStr ""
printClusters (cluster:r) = printCentroid (fromJust (centroidRgb cluster))
                            >> printColorList (clusterColors cluster) 0
                            >> printClusters r
