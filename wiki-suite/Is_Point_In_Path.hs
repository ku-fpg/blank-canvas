{-# LANGUAGE OverloadedStrings #-}
module Is_Point_In_Path where

import Graphics.Blank
import Wiki -- (400,400)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do

    send context $ do
        strokeStyle "blue";
        beginPath();
        rect(100,100,200,200)
        cmds <- sequence [ do
                   b <- isPointInPath (x,y)
                   return $ do
                        beginPath()
                        fillStyle $ if b then "red" else "green"
                        arc(x, y, 5, 0, pi*2, False)
                        fill()
                 | x <- take 8 [25,25+50..]
                 , y <- take 8 [25,25+50..]
                 ]
        stroke()
        -- Now draw the points
        sequence_ cmds

    wiki $ snapShot context "images/Is_Point_In_Path.png"
    wiki $ close context
