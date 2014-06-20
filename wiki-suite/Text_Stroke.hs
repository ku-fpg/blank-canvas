{-# LANGUAGE OverloadedStrings #-}
module Text_Stroke where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        font "60pt Calibri"
        lineWidth 3
        strokeStyle "blue"
        strokeText("Hello World!", 80, 110)


    wiki $ snapShot context "images/Text_Stroke.png"
    wiki $ close context
