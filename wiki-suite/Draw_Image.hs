{-# LANGUAGE OverloadedStrings #-}
module Draw_Image where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        img <- newImage "/images/Haskell.jpg"
        drawImage(img,[69,50])


    wiki $ snapShot context "images/Draw_Image.png"
    wiki $ close context
