module Input where

import Life
import Data.Char
import Control.Monad (forM_)
import Data.Array.Repa (Z(..),(:.)(..),computeP,traverse,extent)

-- Life 1.06 text format input

-- Make pairs of coordinates
pairs :: [Int] -> [(Int,Int)]
pairs [] = []
pairs [x] = []
pairs (x:xs) = (x, head xs) : pairs (tail xs)

-- Read cell coordinates
parseLine :: String -> Maybe [Int]
parseLine cs | or (map isNotNum cs) = Nothing
             | length (words cs) /= 2 = Nothing
             | otherwise = Just $ map readNum (words cs)
    where isNotNum c = not (isDigit c) && not (elem c " -\r") 
          readNum x = read x :: Int

-- Read a pattern file and return a grid with the pattern
parseFile :: FilePath -> Grid -> IO Grid
parseFile fp grid = do
  file <- readFile fp
  let (Z :. w :. h) = extent grid
      ints = map parseLine $ lines file
      coords = map (fmap pairs) ints
  computeP $ traverse grid id (\_ sh@(Z :. a :. b) ->
    if (elem (Just [(a - div w 2, b - div h 2)]) coords)
      then 1 
      else 0)