{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Text (Text)
import Graphics.Blank

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ go

type Ball a = ((Double, Double), Double, a)

type Color = String

epoch :: [Ball ()]
epoch = []

type State = ([Ball Color])

showBall :: (Double, Double) -> Text -> Canvas ()
showBall (x,y) col = do
        beginPath()
        globalAlpha 0.5
        fillStyle col
        arc(x, y, 50, 0, pi*2, False)
        closePath()
        fill()

moveBall :: Ball a -> Ball a
moveBall ((x,y),d,a) = ((x,y+d),d+0.5,a)

go :: DeviceContext -> IO ()
go context = do

     let (w,h) = (width context, height context) :: (Double, Double)
     print (w,h)

     let bounce :: Ball a -> Ball a
         bounce ((x,y),d,a)
            | y + 25 >= h && d > 0 = ((x,y),-(d-0.5)*0.97,a)
            | otherwise         = ((x,y),d,a)

     let loop (balls,cols) = do

             send context $ do
                clearCanvas
                sequence_
                     [ showBall xy col
                     | (xy,_,col) <- balls
                     ]
             threadDelay (20 * 1000) 

             es <- flush context

             let newBalls = [ ((x,y),0,head cols) 
                            | Just (x,y) <- map ePageXY es
                            ]
                      
             loop (map bounce $ map moveBall $ balls ++ newBalls, tail cols)


     loop ([((100,100),0,"blue")],cycle ["red","blue","green","orange","cyan"])
