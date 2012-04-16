module Main where

import Graphics.Blank

main = blankCanvas 3000 $ \ canvas -> do
          let loop (x,y) colors = do
                send canvas $ do
                        save
                        translate (x,y)
                        beginPath
                        moveTo(-100,-100)
                        lineTo(-100,100)
                        lineTo(100,100)
                        lineTo(100,-100)
                        closePath
                        lineWidth 10
                        strokeStyle (head colors)
                        stroke
                        restore

                event <- send canvas $ readEvent MouseDown
                case jsMouse event of
                        Nothing -> loop (x,y) colors
                        Just (x',y') -> loop (fromIntegral x',fromIntegral y') (tail colors)

          (width,height) <- send canvas size
          loop (width / 2,height / 2)
               (cycle [ "#749871", "#1887f2", "#808080", "f01234"])




