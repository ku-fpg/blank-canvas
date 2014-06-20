{-# LANGUAGE OverloadedStrings #-}
module Load_Image_Data_URL where

import Graphics.Blank
import Wiki -- (578,200)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO


main = blankCanvas 3000 $ \ context -> do
    url <- Text.IO.readFile "data/dataURL.txt"
    send context $ do
           img <- newImage url
           drawImage (img,[0,0])

    wiki $ snapShot context "images/Load_Image_Data_URL.png"
    wiki $ close context
