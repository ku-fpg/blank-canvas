{-# LANGUAGE OverloadedStrings #-}
module Image_Loader where

import Graphics.Blank
import Wiki -- (578,400)

main = blankCanvas 3000 { static= ["images/Haskell.jpg","images/House.jpg"] } $ \ context -> do
    send context $ do
        img1 <- newImage "/images/Haskell.jpg"
        img2 <- newImage "/images/House.jpg"
        drawImage(img1,[69,50,97,129])
        drawImage(img2,[200,50])


    wiki $ snapShot context "images/Image_Loader.png"
    wiki $ close context
