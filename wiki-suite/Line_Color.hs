{-# LANGUAGE OverloadedStrings #-}
module Line_Color where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 5
        strokeStyle "#ff0000"
        stroke()


    wiki $ snapShot context "images/Line_Color.png"
    wiki $ close context
