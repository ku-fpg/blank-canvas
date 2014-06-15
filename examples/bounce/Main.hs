{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank
import Data.Time.Clock
import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ go

type Ball a = ((Float,Float),Float,a)

type Color = String

epoch :: [Ball ()]
epoch = []

type State = ([Ball Color])


showBall :: (Float,Float) -> Text -> Canvas ()
showBall (x,y) col = do
--	save()
--	translate (x,y)

        beginPath()
--        scale (1,0.9)
        globalAlpha 0.5
        fillStyle col
        arc(x, y, 50, 0, pi*2, False)
        closePath()
        fill()
        
--	restore()

moveBall :: Ball a -> Ball a
moveBall ((x,y),d,a) = ((x,y+d),d+0.5,a)

go context = do

     (width,height) <- send context size
     print (width,height)

     let bounce :: Ball a -> Ball a
         bounce ((x,y),d,a)
            | y + 25 >= height && d > 0 = ((x,y),-(d-0.5)*0.97,a)
            | otherwise         = ((x,y),d,a)

     let loop (balls,cols) = do
--             print state

             send context $ do
                clearCanvas
                sequence_
                     [ showBall xy col
                     | (xy,_,col) <- balls
                     ]
             threadDelay (20 * 1000)	                   

             es <- flush context
             if (null es) then return () else print es

             let newBalls = [ ((x,y),0,head cols) 
                            | Just (x,y) <- map ePageXY es
                            ]
                      
             loop (map bounce $ map moveBall $ balls ++ newBalls, tail cols)


     loop ([((100,100),0,"blue")],cycle ["red","blue","green","orange","cyan"])
      
