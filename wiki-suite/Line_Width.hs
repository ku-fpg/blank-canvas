{-# LANGUAGE OverloadedStrings #-}
module Line_Width where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 15
        stroke()


    wiki $ snapShot context "images/Line_Width.png"
    wiki $ close context
