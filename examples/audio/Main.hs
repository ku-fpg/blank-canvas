{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

main :: IO ()
main = do
  dat <- getDataDir
  blankCanvas 3000 { root = dat } $ \ context -> do
    send context $ do
      img   <- newImage "images/fan.jpg" --"haskell.png"
      drawImage(img,[0,0])
      _ <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg"
      _ <- newAudio "music/sonata.ogg"
      fillText("test",800,200)
      drawImage(img,[70,50])
