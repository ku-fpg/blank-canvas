{-# LANGUAGE OverloadedStrings #-}
module Image_Crop where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        img <- newImage "/images/Haskell.jpg"
        drawImage(img,[150,0,150,150,50,50,150,200])


    wiki $ snapShot context "images/Image_Crop.png"
    wiki $ close context
