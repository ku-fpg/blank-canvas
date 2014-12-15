{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
--import Paths_wiki_suite

main :: IO ()
main = do
  -- dat <- getDataDir
  blankCanvas 3000 $ \ context -> do
    send context $ do
      img   <- newImage "haskell.png"
      drawImage(img,[69,50])
      sound <- newAudio "http://upload.wikimedia.org/wikipedia/en/d/df/Florence_Foster_Jenkins_H%C3%B6lle_Rache.ogg"
      drawImage(img,[70,50])
