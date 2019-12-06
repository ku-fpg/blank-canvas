{-# LANGUAGE OverloadedStrings #-}
module Draw_Image where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = do
    blankCanvas 3000 $ \ context -> do
        url <- staticURL context "type/jpeg" "images/Haskell.jpg"
        send context $ do
            img <- newImage url
            drawImage(img,[69,50])


        wiki $ snapShot context "images/Draw_Image.png"
        wiki $ close context
