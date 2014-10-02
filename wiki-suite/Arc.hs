{-# LANGUAGE OverloadedStrings #-}
module Arc where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let centerX = width context / 2;
        let centerY = height context / 2;
        let radius = 75;
        let startingAngle = 1.1 * pi
        let endingAngle = 1.9 * pi
        lineWidth 15
        strokeStyle "black"

	beginPath()
        arc(centerX - 50, centerY, radius, startingAngle, endingAngle, False)
        stroke()

	beginPath()
        strokeStyle "blue"
        arc(centerX + 50, centerY, radius, startingAngle, endingAngle, True)
        stroke()


    wiki $ snapShot context "images/Arc.png"
    wiki $ close context
