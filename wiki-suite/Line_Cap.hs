{-# LANGUAGE OverloadedStrings #-}
module Line_Cap where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        sequence_
           [ do beginPath()
                moveTo(200, height context / 2 + n)
                lineTo(width context - 200, height context / 2 + n)
                lineWidth 20
                strokeStyle "#0000ff"
                lineCap cap
                stroke()
           | (cap,n) <- zip ["butt","round","square"] [-50,0,50]
           ]


    wiki $ snapShot context "images/Line_Cap.png"
    wiki $ close context
