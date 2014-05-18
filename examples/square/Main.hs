module Main where

import Graphics.Blank

main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
          let loop (x,y) (color:colors) = do
                send context $ do
                        save()
                        translate (x,y)
                        beginPath()
                        moveTo(-100,-100)
                        lineTo(-100,100)
                        lineTo(100,100)
                        lineTo(100,-100)
                        closePath()
                        lineWidth 10
                        strokeStyle color
                        stroke()
                        restore()

		event <- wait context
                case ePageXY event of
                        Nothing -> loop (x,y) colors
                        Just (x',y') -> loop (fromIntegral x',fromIntegral y') colors

          print "calling size"                
          (width,height) <- send context size
          print ("got size ", (width,height))
          loop (width / 2,height / 2)
               (cycle [ "#749871", "#1887f2", "#808080", "f01234"])




