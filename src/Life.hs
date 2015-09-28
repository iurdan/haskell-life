{-# LANGUAGE QuasiQuotes #-}

module Life where

import Data.Array.Repa (Z(..),(:.)(..),(!))
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Stencil as R
import Data.Array.Repa.Stencil.Dim2
import Data.Array.Repa.Algorithms.Randomish
import System.Random (getStdRandom, random)


type Grid = R.Array R.U R.DIM2 Int

-- Moore neighborhood
sten :: R.Stencil R.DIM2 Int
sten = [stencil2| 1 1 1
                  1 0 1
                  1 1 1 |]

destiny :: Int -> Int -> Int 
destiny _ 3 = 1
destiny x 2 = x
destiny _ _ = 0

-- Next generation
tick :: Grid -> IO Grid
tick world = R.computeP $ R.zipWith destiny world neighbours
    where neighbours = mapStencil2 (R.BoundConst 0) sten world

randomishGrid :: Int -> Int -> IO Grid
randomishGrid w h = do
    seed <- getStdRandom random 
    return $ randomishIntArray (Z :. w :. h) 0 1 seed

zeroGrid :: Int -> Int -> IO Grid
zeroGrid w h = return $ R.fromListUnboxed (Z :. w :. h) (replicate (w*h) 0)
