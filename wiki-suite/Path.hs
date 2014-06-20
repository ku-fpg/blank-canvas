{-# LANGUAGE OverloadedStrings #-}
module Path where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath()
        moveTo(100, 20)
	-- line 1
        lineTo(200, 160)
        -- quadratic curve
        quadraticCurveTo(230, 200, 250, 120)
        -- bezier curve
        bezierCurveTo(290, -40, 300, 200, 400, 150)
        -- line 2
        lineTo(500, 90)
        lineWidth 5
        strokeStyle "blue"
        stroke()



    wiki $ snapShot context "images/Path.png"
    wiki $ close context
