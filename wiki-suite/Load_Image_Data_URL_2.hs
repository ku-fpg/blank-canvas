{-# LANGUAGE OverloadedStrings #-}
module Load_Image_Data_URL_2 where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    url <- readDataURL "image/jpeg" "images/Haskell.jpg"
    send context $ do
           img <- newImage url
           drawImage (img,[0,0])

    wiki $ snapShot context "images/Load_Image_Data_URL_2.png"
    wiki $ close context
