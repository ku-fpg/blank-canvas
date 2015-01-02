{-# LANGUAGE OverloadedStrings #-}
module Rotating_Square where

import Control.Concurrent
import Graphics.Blank
import Wiki -- (384,384)

main :: IO ()
main = blankCanvas 3000 $ flip loop 0

loop :: DeviceContext -> Double -> IO ()
loop context n = do
        send context $ do
                clearRect (0,0,width context,height context)
                beginPath()
                save()
                translate (width context / 2,height context / 2)
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
        v <- wiki $ return (round (n*100) :: Int)
        wiki $ whenM (v `mod` 2 == 0) $ do
                file <- wiki $ anim_png "Rotating_Square"
                wiki $ snapShot context $ file
                wiki $ whenM (v == 48) $ do { build_anim "Rotating_Square" 5; close context }
        loop context (n + 0.01)

