{-# LANGUAGE OverloadedStrings #-}
module Bezier_Curve where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath()
        moveTo(188, 150)
        bezierCurveTo(140, 10, 388, 10, 388, 170)
        lineWidth 10
        -- line color
        strokeStyle "black"
        stroke()


    wiki $ snapShot context "images/Bezier_Curve.png"
    wiki $ close context
