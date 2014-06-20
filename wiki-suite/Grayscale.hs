{-# LANGUAGE OverloadedStrings #-}
module Grayscale where

import Graphics.Blank

main = blankCanvas 3000 { static= ["images/Haskell.jpg","images/House.jpg"] } $ \ context -> do
    send context $ do
        img <- newImage "/images/House.jpg"
        drawImage(img,[50,50])
	