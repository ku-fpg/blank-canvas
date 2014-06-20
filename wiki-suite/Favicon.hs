{-# LANGUAGE OverloadedStrings #-}
module Favicon where

import Graphics.Blank                     -- import the blank canvas
import Wiki -- (32,32)

main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context

             beginPath()
	     let relPath x xs = do
	     	      beginPath()
		      moveTo x
		      sequence_ [ lineTo p | ss <- drop 2 $ scanl (flip (:)) [] (x:xs)
		      		  	   , let p = (sum (map fst ss), sum (map snd ss))
					   ]
		      closePath()
		      
             translate(height context * 0.07,height context * 0.8)
	     scale (height context / 20,-height context / 20)

	     let shape x xs col = do
	     	     relPath x xs
		     lineWidth 0.5
		     fillStyle col
		     lineCap "round"
		     fill()                     

             shape (0,0) [(4,6),(-4,6),(3,0),(4,-6),(-4,-6)] "#E8000D" 
	     shape (8,6) [(-4,6),(3,0),(8,-12),(-3,0),(-2.5,3.75),(-2.5,-3.75),(-3,0)] "#0022B4"
	     shape (17,3.5) [(-3.5,0),(-8/6,12/6),(3.5+8/6,0)] "#FFC82D" 
	     shape (17,6.5) [(-5.5,0),(-8/6,12/6),(5.5+8/6,0)] "#FFC82D" 

        -- wiki $ txt <- send context $ toDataURL() ; print txt
        wiki $ snapShot context "images/Favicon.png"
        wiki $ close context
