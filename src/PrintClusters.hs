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

printCentroid :: Color -> IO ()
printCentroid centroid = putStrLn "--" >>
                        print (fromJust (rgb centroid))

printClusters :: [Cluster] -> IO ()
printClusters [] = putStr ""
printClusters (cluster:r) = printCentroid (centroidColor cluster)
                            >> printColorList (clusterColors cluster) 0
                            >> printClusters r
