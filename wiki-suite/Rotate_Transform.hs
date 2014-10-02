{-# LANGUAGE OverloadedStrings #-}
module Rotate_Transform where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let rectWidth = 150;
        let rectHeight = 75;
        translate(width context / 2, height context / 2);
	rotate (pi/4);

        fillStyle "blue";
        fillRect(rectWidth / (-2), rectHeight / (-2), rectWidth, rectHeight);


    wiki $ snapShot context "images/Rotate_Transform.png"
    wiki $ close context
