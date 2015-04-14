{-# LANGUAGE OverloadedStrings #-}
module Quadratic_Curve where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath()
        moveTo(188, 150)
        quadraticCurveTo(288, 0, 388, 150)
        lineWidth 10
        -- line color
        strokeStyle "black"
        stroke()


    wiki $ snapShot context "images/Quadratic_Curve.png"
    wiki $ close context
