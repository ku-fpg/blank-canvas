{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Data.Text (Text)
import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

main :: IO ()

main = do
  dat <- getDataDir    
  blankCanvas 3000 { root = dat, events = ["mousedown"]} $ go

type Ball a = ((Double, Double), Double, a)

type Color = String

epoch :: [Ball ()]
epoch = []

type State = ([Ball Color])

showBall :: (Double, Double) -> Text -> Canvas ()
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

go :: DeviceContext -> IO b
go context = do

     let (w,h) = (width context, height context) :: (Double, Double)
     print (w,h)

     let bounce :: Ball a -> Ball a
         bounce ((x,y),d,a)
            | y + 25 >= h && d > 0 = ((x,y),-(d-0.5)*0.97,a)
            | otherwise         = ((x,y),d,a)

     let hit :: Ball a -> Bool
         hit ((_,y),_,_)
            | y + 25 >= h          = True
            | otherwise            = False

     let loop (balls,cols) = do
--             print state

             send context $ do
                clearCanvas
                sequence_
                     [ showBall xy col
                     | (xy,_,col) <- balls
                     ]
                let x = map hit $ balls
                    y = elem True x
                sound <- newAudio "music/ballbounce.wav"
                when y $ playAudio sound -- When a ball is at the bottom of the Canvas, play the bounce sound

             threadDelay (20 * 1000)	                   

             es <- flush context
             if (null es) then return () else print es

             let newBalls = [ ((x,y),0,head cols) 
                            | Just (x,y) <- map ePageXY es
                            ]
                      
             loop (map bounce $ map moveBall $ balls ++ newBalls, tail cols)


     loop ([((100,100),0,"blue")],cycle ["red","blue","green","orange","cyan"])
