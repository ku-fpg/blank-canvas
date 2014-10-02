{-# LANGUAGE OverloadedStrings #-}
module Square where

import Graphics.Blank
import Wiki -- (512,512)

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
          let loop (x,y) (color:colors) = do
                send context $ saveRestore $ do
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


                wiki $ counter (\ _ -> True) $ \ n -> do
                    file <- wiki $ anim_png "Square"
                    wiki $ snapShot context $ file
                    let e (x',y') = wiki $ send context $ trigger $ Event {
                        eMetaKey = False
                      , ePageXY = return (x',y')
                      , eType = "mousedown"
                      , eWhich = Nothing
                    }
                    wiki $ whenM (n == 1) $ e (x-100,y+50)
                    wiki $ whenM (n == 2) $ e (x+20, y-60)
                    wiki $ whenM (n == 3) $ e (x+40, y+20)
                    wiki $ whenM (n == 4) $ e (x+60, y-60)
                    wiki $ whenM (n >= 5) $ do { build_anim "Square" 100; close context }

                event <- wait context
                case ePageXY event of
                        Nothing -> loop (x,y) colors
                        Just (x',y') -> loop (x',y') colors

          putStrLn "calling size"                
          loop (width context / 2,height context / 2)
               (cycle [ "#749871", "#1887f2", "#808080", "f01234"])




