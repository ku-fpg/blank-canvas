{-# LANGUAGE OverloadedStrings #-}
module Circle where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let centerX = width context / 2
        let centerY = height context / 2
        let radius = 70

        beginPath()
        arc(centerX, centerY, radius, 0, 2 * pi, False)
        fillStyle "#8ED6FF"
        fill()
        lineWidth  5
        strokeStyle "black"
        stroke()


    wiki $ snapShot context "images/Circle.png"
    wiki $ close context
