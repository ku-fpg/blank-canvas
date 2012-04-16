module Main where

import Graphics.Blank

main = blankCanvas 3000 $ \ canvas -> do
          let loop (x,y) (color:colors) = do
                send canvas $ do
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

                event <- send canvas $ readEvent MouseDown
                case jsMouse event of
                        Nothing -> loop (x,y) colors
                        Just (x',y') -> loop (fromIntegral x',fromIntegral y') colors

          (w,h) <- send canvas $ do
                        w <- width
                        h <- height
                        return (w,h)

          loop (w / 2,h / 2)
               (cycle [ "#749871", "#1887f2", "#808080", "f01234"])




