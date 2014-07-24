{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
     loop context 0

loop :: DeviceContext -> Float -> IO a
loop context n = do
        send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                beginPath()
                save()
                translate (w / 2, h / 2)
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
        threadDelay (20 * 1000)	
        loop context (n + 0.01)