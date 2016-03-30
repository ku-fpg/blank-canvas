{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as Text
import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

-- A bare-bones demonstration of loading and playing/pausing an audio file.
main :: IO ()
main = do
  dat <- getDataDir
  blankCanvas 3000 { events = ["mousedown"], root = dat } $ \context ->
    -- The Audio data type works with both local files and URLs
    -- startLoop context "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg"
    startLoop context "music/sonata.ogg"
         
data Play = Playing | Paused
          deriving (Eq,Ord,Show)

swap :: Play -> Play
swap Playing = Paused
swap Paused  = Playing

startLoop context filename = do
  music <- send context $ newAudio filename
  loop context music Playing
  
loop :: DeviceContext -> CanvasAudio -> Play -> IO ()
loop context audio play = do
  send context $ do
    let (w,h) = (width context, height context)
    clearRect (0,0,w,h)
    lineWidth 1
    font "30pt Calibri"

    -- This music sure is loud, better make it a bit softer.
    setVolumeAudio(audio,0.9)
    
    -- Everyone likes faster music, let's make it twice as fast
    setPlaybackRateAudio(audio,2.0)

    -- Play/pause the audio depend in which state it's in
    if (play == Playing)
      then do
        fillText("Click screen to play audio",50,50)
        pauseAudio audio
      else do
        fillText("Click screen to pause audio",50,50)
        playAudio audio

    -- display the current time
    -- TODO: use threading so that the current time can continuously update while
    --   waiting for a mouse click
    time <- currentTimeAudio audio
    fillText(Text.append "Current Time: " (Text.pack $ show time),50,90)

  -- wait for mouse click
  event <- wait context
  case ePageXY event of
    -- if no mouse location, ignore, and redraw
    Nothing -> loop context audio play
    Just (_,_) -> loop context audio (swap play)
