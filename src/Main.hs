module Main where

import Graphics.UI.SDL as SDL
import Control.Monad (forM_)
import Data.IORef
import System.Environment
import System.Exit (exitSuccess)
import Data.Array.Repa (Z(..),(:.)(..),(!))
import qualified Data.Array.Repa as R
import Life
import Input


draw :: Int -> Grid -> Surface -> IO ()
draw s grid surface = do
  let (Z :. w :. h) = R.extent grid
  forM_ [(x,y) | x <- [0..(w-1)], y <- [0..(h-1)]] $ \(x,y) -> do
    let rect = Rect (x*s) (y*s) s s
        drawCell = fillRect surface (Just rect) . Pixel
    drawCell $ case grid ! (Z :. x :. y) of
      1 -> 0x34be5b
      0 -> 0x2c2c2c

-- Toggle the cell in the given position
modifyCell :: (Int, Int) -> Grid -> IO Grid
modifyCell (x,y) grid = do
  R.computeP $ R.traverse grid id (\_ sh@(Z :. a :. b) ->
    if ((a,b) == (x,y))
      then case grid ! sh of
        0 -> 1
        _ -> 0
      else grid ! sh)

-- Set the cell in the given position to 1
liveCell :: (Int,Int) -> Grid -> IO Grid
liveCell (x,y) grid = do
  R.computeP $ R.traverse grid id (\_ sh@(Z :. a :. b) ->
    if ((a,b) == (x,y))
      then 1
      else grid ! sh)

editMode :: Int -> Surface -> IORef Grid -> IORef Bool -> IO ()
editMode s surface gridR drawR = do
  info <- getVideoInfo
  -- Video fits the screen size
  let w = videoInfoWidth info
      h = videoInfoHeight info

  e <- SDL.waitEvent
  case e of
    Quit -> exitSuccess
    -- Toggle a cell when it is clicked
    MouseButtonDown x y _ -> do
      grid <- readIORef gridR
      modifyCell ((fromIntegral x `div` s),(fromIntegral y `div` s)) grid
          >>= writeIORef gridR
      grid <- readIORef gridR
      draw s grid surface
      SDL.flip surface
      writeIORef drawR True 
      editMode s surface gridR drawR
    MouseButtonUp x y _ -> do
      writeIORef drawR False
      editMode s surface gridR drawR
    MouseMotion x y _ _ -> do
      dr <- readIORef drawR
      if dr
        then do
          grid <- readIORef gridR
          liveCell ((fromIntegral x `div` s),(fromIntegral y `div` s)) grid
              >>= writeIORef gridR
          grid <- readIORef gridR
          draw s grid surface
          SDL.flip surface
          editMode s surface gridR drawR
        else
          editMode s surface gridR drawR
    KeyDown (Keysym SDLK_r _ _) -> do
      writeIORef gridR =<< randomishGrid (w `div` s) (h `div` s)
      mainLoop s surface gridR
    -- Kill all cells when key 'c' is pressed
    KeyDown (Keysym SDLK_c _ _) -> do
      zeroGrid (w `div` s) (h `div` s) >>= \g -> do
        writeIORef gridR g
        draw s g surface
      SDL.flip surface
      editMode s surface gridR drawR
    -- Advance a generation when key N pressed
    KeyDown (Keysym SDLK_n _ _) -> do
      grid <- tick =<< readIORef gridR
      writeIORef gridR grid
      draw s grid surface
      SDL.flip surface
      editMode s surface gridR drawR
    -- Exit edit mode when SPACE is pressed
    KeyDown (Keysym SDLK_SPACE _ _) -> mainLoop s surface gridR
    -- Exit full screen and program when RETURN is pressed
    KeyDown (Keysym SDLK_RETURN _ _) -> do
      toggleFullscreen surface
      exitSuccess
    _ -> editMode s surface gridR drawR
    
mainLoop :: Int -> Surface -> IORef Grid -> IO ()
mainLoop s surface gridR = do
  grid <- readIORef gridR
  draw s grid surface
  -- Advance to next generation
  writeIORef gridR =<< tick grid
  SDL.flip surface

  e <- SDL.pollEvent
  case e of 
    Quit -> exitSuccess
    -- Exit main loop and enter edit mode when SPACE is pressed
    KeyDown (Keysym SDLK_SPACE _ _) -> do
        drawR <- newIORef False
        editMode s surface gridR drawR
    -- Exit full screen and program when RETURN is pressed
    KeyDown (Keysym SDLK_RETURN _ _) -> do
      toggleFullscreen surface
      exitSuccess
    _ -> do
      -- 50 frames / second
      SDL.delay 20
      mainLoop s surface gridR

main :: IO ()
main = do
  args <- getArgs
  SDL.init [SDL.InitEverything]
  info <- getVideoInfo
  -- Video fits the screen size
  let w = videoInfoWidth info
      h = videoInfoHeight info
      -- First argument is cell size,
      -- if any argument is given default size is 8
      s = if length args > 0 
            then read (head args)
            else 8
  zeros <- zeroGrid (w `div` s) (h `div` s)
  gridR <- newIORef zeros
  if length args > 1
    then 
      -- Second argument is pattern file path
      writeIORef gridR =<< parseFile (args !! 1) zeros
    else
      writeIORef gridR zeros
  SDL.setCaption "Conway's Game of Life" "Game of Life"
  setVideoMode w h 32 [Fullscreen]
  surface <-  getVideoSurface
  grid <- readIORef gridR
  draw s grid surface
  SDL.flip surface
  drawR <- newIORef False
  editMode s surface gridR drawR
