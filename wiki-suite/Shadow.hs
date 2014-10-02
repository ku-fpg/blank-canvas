{-# LANGUAGE OverloadedStrings #-}
module Shadow where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        rect(188, 40, 200, 100);
        fillStyle "red";
        shadowColor "#999";
        shadowBlur 20;
        shadowOffsetX 15;
        shadowOffsetY 15;
        fill()


    wiki $ snapShot context "images/Shadow.png"
    wiki $ close context
