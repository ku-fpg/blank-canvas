{-# LANGUAGE OverloadedStrings #-}
module Favicon where

import Graphics.Blank                     -- import the blank canvas
import Wiki -- (32,32)

main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context

             beginPath()
             moveTo(16,1)
             lineTo(8,32)
             lineWidth 2
             strokeStyle "blue"
             lineCap "round"
             stroke()                     

             beginPath()
             moveTo(16,1)
             lineTo(16+8,32)
             lineWidth 2
             strokeStyle "blue"
             lineCap "round"
             stroke()                     

	     beginPath()
	     rect(0.5,1.5,31,22)
	     fillStyle "white"
	     fill()
	     lineWidth 1
	     strokeStyle "red"
	     stroke()

             beginPath()
             moveTo(4,25.5)
             lineTo(32-4,25.5)
             lineWidth 3
             strokeStyle "black"
             lineCap "butt"
             stroke()                     

             lineWidth 1.2

             strokeStyle "black"
             font "7pt Chalkboard"
             textBaseline "top"
             textAlign "center"
             strokeText("Blank",16,1)
             
             strokeStyle "black"
             font "7pt Chalkboard"
             textBaseline "top"
             textAlign "center"
             strokeText("Canvas",16,11)             

        -- wiki $ txt <- send context $ toDataURL() ; print txt
        wiki $ snapShot context "images/Favicon.png"
        wiki $ close context
