{-# LANGUAGE OverloadedStrings #-}
module Load_Image_Data_URL_2 where

import Graphics.Blank
import Wiki -- (578,200)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO


main = blankCanvas 3000 $ \ context -> do
    url <- readDataURL "image/jpeg" "images/Haskell.jpg"
    send context $ do
           img <- newImage url
           drawImage (img,[0,0])

    wiki $ snapShot context "images/Load_Image_Data_URL_2.png"
    wiki $ close context
