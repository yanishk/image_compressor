{-
-- EPITECH PROJECT, 2024
-- imagecomp
-- File description:
-- OpenFile
-}

module OpenFile (Pixel(..), parseFiletoStruct, defaultPixel, checkAllTuples) where

import Text.Read (readMaybe)

data Pixel =
    Pixel {
        pos :: Maybe (Int, Int),
        rgb :: Maybe (Int, Int, Int)
    } deriving (Show, Eq)

defaultPixel :: Pixel
defaultPixel = Pixel
    { pos = Nothing,
      rgb = Nothing
    }

isAShort :: Int -> Bool
isAShort nb
    | nb >= 0 && nb <= 255 = True
    | otherwise = False

checkTuple :: (Int, Int, Int) -> Bool
checkTuple (x, y, z)
    | isAShort x && isAShort y && isAShort z = True
    | otherwise = False

checkAllTuples :: [Pixel] -> Bool
checkAllTuples pixels = all checkPixel pixels
    where
        checkPixel :: Pixel -> Bool
        checkPixel pixel = case rgb pixel of
            Just color -> checkTuple color
            Nothing -> False

replaceSpecialChars :: Char -> Char
replaceSpecialChars c
  | c `elem` ",()" = ' '
  | otherwise = c

cleanString :: String -> String
cleanString str = filter isAllowedChar (map replaceSpecialChars str)
  where
    isAllowedChar c = c `elem` "0123456789, "

makeDouble :: String -> Maybe (Int, Int)
makeDouble str = case map readMaybe (words (cleanString str)) of
    [Just a, Just b] -> Just (a, b)
    _ -> Nothing

makeTriple :: String -> Maybe (Int, Int, Int)
makeTriple str = case map readMaybe (words (cleanString str)) of
    [Just a, Just b, Just c] -> Just (a, b, c)
    _ -> Nothing

putIntoData :: [String] -> [Pixel]
putIntoData [] = []
putIntoData str =
    let p = makeDouble (head str)
        c = makeTriple (head (tail str))
    in case (p, c) of
        (Just posCpy, Just rgbCpy) -> [defaultPixel {pos = Just posCpy,
            rgb = Just rgbCpy}]
        _ -> []

parseFiletoStruct :: [String] -> [Pixel]
parseFiletoStruct [] = []
parseFiletoStruct str = putIntoData (words (head str))
    ++ parseFiletoStruct (tail str)
