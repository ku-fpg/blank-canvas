module Main where

import Graphics.Blank

main = blankCanvas 3000 $ \ context -> do
        send context $ do
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()

