{-# LANGUAGE OverloadedStrings #-}
module Text_Color where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        font "40pt Calibri"
        fillStyle "#0000ff"
        fillText("Hello World!", 150, 100)


    wiki $ snapShot context "images/Text_Color.png"
    wiki $ close context
