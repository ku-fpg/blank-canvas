{-# LANGUAGE OverloadedStrings #-}
module Line where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath()
        moveTo(100,150)
        lineTo(450,50)
        stroke()


    wiki $ snapShot context "images/Line.png"
    wiki $ close context
