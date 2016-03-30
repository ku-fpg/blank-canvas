{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad (when)
import Data.Maybe (isJust)
import Data.Text (Text)
import Graphics.Blank
import Paths_blank_canvas_examples (getDataDir)

main :: IO ()

main = do
  dat <- getDataDir
  blankCanvas 3000 { root = dat, events = ["mousedown"]} $ go

type Ball a = ((Double, Double), Double, a)

type Color = String

-- returns a list of all the separate audio files - mapping to a pentatonic scale
getAudios :: DeviceContext -> IO [CanvasAudio]
getAudios context = do
  root   <- send context $ newAudio "music/ballbounce_root.wav"
  second <- send context $ newAudio "music/ballbounce_second.wav"
  third  <- send context $ newAudio "music/ballbounce_third.wav"
  fifth  <- send context $ newAudio "music/ballbounce_fifth.wav"
  sixth  <- send context $ newAudio "music/ballbounce_sixth.wav"
  return [root,second,third,fifth,sixth]

-- each color is mapped to a specific pitch in the pentatonic scale
colorToAudio :: [CanvasAudio] -> Maybe (Text) -> CanvasAudio
colorToAudio as (Just c)
  | c == "red"     = as !! 0
  | c == "blue"    = as !! 1
  | c == "green"   = as !! 2
  | c == "orange"  = as !! 3
  | c == "cyan"    = as !! 4
  | otherwise      = as !! 0
colorToAudio as Nothing = as !! 0

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

go :: DeviceContext -> IO b
go context = do

     let (_w,h) = (width context, height context) :: (Double, Double)

     audios <- getAudios context
     
     let bounce :: Ball a -> Ball a
         bounce ((x,y),d,a)
            | y + 25 >= h && d > 0 = ((x,y),-(d-0.5)*0.97,a)
            | otherwise         = ((x,y),d,a)

     -- Determines whether the ball has hit the bottom of the screen
     let hit :: Ball a -> Bool
         hit ((_,y),_,_)
            | y + 25 >= h          = True
            | otherwise            = False

     let hit :: Ball a -> Maybe a 
         hit ((_,y),_,a)
            | y + 25 >= h          = Just a
            | otherwise            = Nothing

     let loop (balls,cols) = do
             send context $ do
                clearCanvas
                sequence_
                     [ showBall xy col
                     | (xy,_,col) <- balls
                     ]
                let x       = map hit $ balls
                    -- all values that are hitting the bottom of the screen in this frame
                    ys      = filter (\a -> isJust a) x
                    -- get the associated notes from the balls that are hitting the bottom of the screen
                    yssound = map (colorToAudio audios) ys

                mapM_ playAudio yssound -- play all the sounds

             threadDelay (20 * 1000) -- delay the redrawing of frame (keeps things smooth)

             es <- flush context
             if (null es) then return () else print es

             let newBalls = [ ((x,y),0,head cols) 
                            | Just (x,y) <- map ePageXY es
                            ]
                      
             loop (map bounce $ map moveBall $ balls ++ newBalls, tail cols)


     loop ([((100,100),0,"blue")],cycle ["red","blue","green","orange","cyan"])
