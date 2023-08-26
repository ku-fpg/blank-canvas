{-# LANGUAGE OverloadedStrings #-}
module Bounce where

import Control.Concurrent
import Control.Monad -- wiki $
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Stream.Infinite as Stream
import Data.Stream.Infinite (Stream)
import Data.Text (Text)
import Graphics.Blank
import Wiki -- (512,384)

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
     let bounce :: Ball a -> Ball a
         bounce ((x,y),d,a)
            | y + 25 >= height context && d > 0 = ((x,y),-(d-0.5)*0.97,a)
            | otherwise         = ((x,y),d,a)

     let loop :: ([Ball Text], Stream Text) -> IO ()
         loop (balls,cols) = do

             send context $ do
                clearCanvas
                sequence_
                     [ showBall xy col
                     | (xy,_,col) <- balls
                     ]
             threadDelay (20 * 1000)

             wiki $ counter (\ _ -> True) $ \ n -> do
                  file <- wiki $ anim_png "Bounce"
                  wiki $ when (n `mod` 43 == 0) $ send context $ trigger $ Event { eMetaKey = False, ePageXY = return(fromIntegral (100 + length balls * 45),20), eType = "mousedown", eWhich = Nothing}
                  wiki $ when (n `mod` 3 == 0) $ snapShot context $ file
                  wiki $ when (n >= 400) $ do { build_anim "Bounce" 7; close context }

             es <- flush context
             if (null es) then return () else print es

             let newBalls = [ ((x,y),0,Stream.head cols)
                            | Just (x,y) <- map ePageXY es
                            ]

             loop (map bounce $ map moveBall $ balls ++ newBalls, Stream.tail cols)


     loop ([((100,100),0,"blue")],Stream.cycle ("red" :| ["blue","green","orange","cyan"]))
