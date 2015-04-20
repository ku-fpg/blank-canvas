{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

-- A bare-bones demonstration of loading and playing an audio file.
main :: IO ()
main = do
  dat <- getDataDir
  blankCanvas 3000 { root = dat } $ \ context -> do
    send context $ do
      -- The Audio data type works with both local files and URLs
      _     <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg"
      music <- newAudio "music/sonata.ogg"
      playAudio music
