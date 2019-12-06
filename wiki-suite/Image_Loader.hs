{-# LANGUAGE OverloadedStrings #-}
module Image_Loader where

import Graphics.Blank
import Wiki -- (578,400)

main :: IO ()
main = do
    blankCanvas 3000 $ \ context -> do
        url1 <- staticURL context "type/jpeg" "images/Haskell.jpg"
        url2 <- staticURL context "type/jpeg" "images/House.jpg"
        send context $ do
            img1 <- newImage url1
            img2 <- newImage url2
            drawImage(img1,[69,50,97,129])
            drawImage(img2,[200,50])
        
        
        wiki $ snapShot context "images/Image_Loader.png"
        wiki $ close context
