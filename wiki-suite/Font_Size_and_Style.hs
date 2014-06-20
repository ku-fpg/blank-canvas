{-# LANGUAGE OverloadedStrings #-}
module Font_Size_and_Style where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        font "40pt Calibri"
        fillText("Hello World!", 150, 100)


    wiki $ snapShot context "images/Font_Size_and_Style.png"
    wiki $ close context
