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

printPixel :: Pixel -> IO ()
printPixel pixel = putStr (show (fromJust (pos pixel)))
                    >> putStr " " >> print (fromJust (rgb pixel))

printPixelList :: Maybe [Pixel] -> Int -> IO ()
printPixelList (Just []) _ = return ()
printPixelList Nothing _ = putStrLn "-"
printPixelList pixels 0 = putStrLn "-" >> printPixelList pixels 1
printPixelList (Just (pixel:rest)) n = printPixel pixel >>
                                        printPixelList (Just rest) (n + 1)

printCentroid :: (Int, Int, Int) -> IO ()
printCentroid centroid = putStrLn "--" >> print centroid

printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters (cluster:r) = printCentroid (fromJust (centroidRgb cluster))
                            >> printPixelList (clusterPixels cluster) 0
                            >> printClusters r
