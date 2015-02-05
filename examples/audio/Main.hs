{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

main :: IO ()
main = do
  dat <- getDataDir
  blankCanvas 3000 { root = dat } $ \ context -> do
    send context $ do
      img   <- newImage "images/fan.jpg" 
      drawImage(img,[0,0])
      -- The Audio data type works with both local files and URLs
      -- florence <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg"
      florence <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache1.ogg"      
      _ <- newAudio "music/sonata.ogg"
      fillText("test",800,200)
      play florence
      drawImage(img,[70,50])
