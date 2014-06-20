{-# LANGUAGE OverloadedStrings #-}
module Draw_Canvas where

import Graphics.Blank                     -- import the blank canvas
import Wiki -- (600,200)

main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()                  -- this draws the ink into the canvas

		me <- myCanvasContext
		drawImage(me,[50,50])

        wiki $ snapShot context "images/Draw_Canvas.png"
        wiki $ close context
