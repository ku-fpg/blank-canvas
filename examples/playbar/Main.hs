{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as Text
import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

import Debug.Trace

-- a play bar with play/pause functionality 
main :: IO ()
main = do
  dat <- getDataDir
  blankCanvas 3000 { events = ["mousedown"], root = dat } $ \context ->
    startLoop context "music/sonata.ogg"
         
data Play = Playing | Paused
          deriving (Eq,Ord,Show)

-- switch Play to opposite value
swap :: TVar Play -> STM ()
swap play = do
  play' <- readTVar play
  if (play' == Playing)
    then writeTVar play (Paused)
    else writeTVar play (Playing)

-- starts the loop.
-- divided into three threads:
-- getCurTime: updates the current time into the curTime TVar, this is its
--   own thread to keep the latency from retrieving the current time separate
--   from the updating of the frame
-- loopInput: get the mouse input
-- loopBar: draw the frame
startLoop :: DeviceContext -> Text.Text -> IO ()
startLoop context filename = do
  music   <- send context $ newAudio filename -- make new Audio
  play    <- newTVarIO (Paused) -- The audio is paused when the page loads
  curTime <- newTVarIO 0 -- The audio starts at the beginning

  forkIO $ getCurTime context music curTime
  forkIO $ loopInput context music play
  loopBar context music play curTime
  
-- Draw the pause symbol, ||
pauseDraw :: Double -> Double -> Canvas ()
pauseDraw x y = do
  lineWidth 5
  lineCap "round"

  -- left line
  beginPath()
  moveTo(x+4,y)
  lineTo(x+4,y+31)
  stroke()

  -- right line
  beginPath()
  moveTo(x+17,y)
  lineTo(x+17,y+31)
  stroke()

-- The play symbol |>
playDraw :: Double -> Double -> Canvas ()
playDraw x y = do
  lineWidth 5
  lineCap "round"

  beginPath()
  moveTo(x,y)
  lineTo(x,y+31)
  stroke()
  beginPath()
  moveTo(x,y+31)
  lineTo(x+23,y+16)
  stroke()
  beginPath()
  moveTo(x+23,y+16)
  lineTo(x,y)
  stroke()


-- draws the progress bar and fills it in as
-- audio progresses
playbarBox :: Double -> Double -> Double -> Double -> Canvas ()
playbarBox x y curTime time = do
  -- emty bar
  beginPath ()
  rect(x+32,y,160,31)
  lineWidth 5
  stroke()

  -- fill in
  beginPath ()
  let proportion = curTime / time
  rect(x+32,y,160*proportion,30)
  fill()
  lineWidth 0
  stroke()

-- draws the entire playbar with play/pause
--   symbol and progress bar
playbarDraw context audio play curTime = do
  send context $ do
    -- clear frame
    clearRect (0,0,200,45)

    -- get full duration of audio file
    let time = durationAudio audio
    playbarBox 5 10 curTime time

    -- draw play/pause depending on current state
    if (play == Playing)
      then do
      pauseDraw 5 10
      else do
      playDraw 5 10

-- controls the drawing/redrawing of the frames    
loopBar :: DeviceContext -> CanvasAudio -> TVar Play -> TVar Double -> IO ()
loopBar context audio play curTime = do
  play'    <- readTVarIO play
  curTime' <- readTVarIO curTime
  playbarDraw context audio play' curTime'
  threadDelay (40 * 1000)
  loopBar context audio play curTime

-- continuously updates the TVar Double with the current time
getCurTime :: DeviceContext -> CanvasAudio -> TVar Double -> IO ()
getCurTime context audio curTime = do
  curTime' <- send context $ currentTimeAudio audio
  atomically $ writeTVar curTime curTime'
  getCurTime context audio curTime

-- reads the mouse input
loopInput :: DeviceContext -> CanvasAudio -> TVar Play -> IO ()
loopInput context audio play = do
  play' <- readTVarIO play
  event <- wait context
  case ePageXY event of
    -- if no mouse location, ignore, and loop again
    Nothing -> loopInput context audio play
    -- rework to get proper clicking range
    Just (w,h) -> do
      -- checks to see if the mouse is being clicked on top of the play/pause button
      if (w >= 5 && w <= 28 && h >= 10 && h <= 41) then
        send context $ do
        if (play' == Playing)
          then do
          pauseAudio audio
          else do
          playAudio audio
        else loopInput context audio play

      -- update play
      atomically $ swap play
    
  loopInput context audio play
