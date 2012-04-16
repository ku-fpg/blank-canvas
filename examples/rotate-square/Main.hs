module Main where

import Graphics.Blank

main = blankCanvas 3000 $ \ canvas -> loop canvas (0 :: Float)
 where
    loop canvas n = do

        send canvas $ do
                w <- width
                h <- height
                clearRect (0,0,w,h)
                beginPath()
                save()
                translate (w / 2,h / 2)
                rotate (pi * n)
                beginPath()
                moveTo(-100,-100)
                lineTo(-100,100)
                lineTo(100,100)
                lineTo(100,-100)
                closePath()
                lineWidth 10
                strokeStyle "green"
                stroke()
                restore()

        loop canvas (n + 0.01)

