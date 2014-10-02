{-# LANGUAGE OverloadedStrings #-}
module Red_Line where

import Graphics.Blank                     -- import the blank canvas
import Wiki -- (600,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()                  -- this draws the ink into the canvas

        wiki $ snapShot context "images/Red_Line.png"
        wiki $ close context
