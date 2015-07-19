module Input where

import Life
import Data.Char
import Data.Array.Repa (Z(..),(:.)(..),computeP,traverse,extent)

-- Life 1.06 text format input

-- Read cell coordinates
parseLine :: String -> Maybe (Int,Int)
parseLine cs | or (map isNotNum cs) = Nothing
             | length (words cs) /= 2 = Nothing
             | otherwise = Just $ (\[a,b] -> (read a, read b)) (words cs)
    where isNotNum c = not (isDigit c) && not (elem c " -\r") 

-- Read a pattern file and return a grid with the pattern
parseFile :: FilePath -> Grid -> IO Grid
parseFile fp grid = do
  file <- readFile fp
  let (Z :. w :. h) = extent grid
      coords = map parseLine $ lines file
  computeP $ traverse grid id (\_ sh@(Z :. a :. b) ->
    if (elem (Just (a - div w 2, b - div h 2)) coords)
      then 1 
      else 0)