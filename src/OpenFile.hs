{-
-- EPITECH PROJECT, 2024
-- imagecomp
-- File description:
-- OpenFile
-}

module OpenFile (Color(..), parseFiletoStruct, defaultColor) where

import Text.Read (readMaybe)

data Color =
    Color {
        pos :: Maybe (Int, Int),
        rgb :: Maybe (Int, Int, Int)
    } deriving (Show, Eq)

defaultColor :: Color
defaultColor = Color
    { pos = Nothing,
      rgb = Nothing
    }

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

putIntoData :: [String] -> [Color]
putIntoData [] = []
putIntoData str =
    let p = makeDouble (head str)
        c = makeTriple (head (tail str))
    in case (p, c) of
        (Just posCpy, Just rgbCpy) -> [defaultColor {pos = Just posCpy,
            rgb = Just rgbCpy}]
        _ -> []


parseFiletoStruct :: [String] -> [Color]
parseFiletoStruct [] = []
parseFiletoStruct str = putIntoData (words (head str))
    ++ parseFiletoStruct (tail str)
