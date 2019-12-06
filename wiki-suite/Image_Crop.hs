{-# LANGUAGE OverloadedStrings #-}
module Image_Crop where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = do
    blankCanvas 3000 $ \ context -> do
        url <- staticURL context "type/jpeg" "images/Haskell.jpg"
        send context $ do
            img <- newImage url
            drawImage(img,[150,0,150,150,50,50,150,200])
        
        
        wiki $ snapShot context "images/Image_Crop.png"
        wiki $ close context
