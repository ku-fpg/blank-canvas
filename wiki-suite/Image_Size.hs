{-# LANGUAGE OverloadedStrings #-}
module Image_Size where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        img <- newImage "/images/Haskell.jpg"
        drawImage(img,[69,50,97,129])


    wiki $ snapShot context "images/Image_Size.png"
    wiki $ close context
