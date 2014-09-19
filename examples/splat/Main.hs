{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import Graphics.Blank.GHCi

test :: Double -> Canvas ()
test n = do
    context <- myCanvasContext
    let (w, h) = (width context, height context)
    save()
    translate (w / 2, h/ 2)
    rotate (pi * n)
    beginPath()
    moveTo(-200,-200)
    lineTo(-200,200)
    lineTo(200,200)
    lineTo(200,-200)
    closePath()
    lineWidth 25
    strokeStyle "orange"
    stroke()
    restore()

splat :: Canvas () -> IO ()
splat = splatCanvas 3000

main :: IO ()
main = do
    splat $ test 0
    splat $ test 0.1
    splat $ test 0.2
    splat $ test 0.3
    putStrLn "only works inside ghci"

