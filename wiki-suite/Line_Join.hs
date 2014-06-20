{-# LANGUAGE OverloadedStrings #-}
module Line_Join where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        
        lineWidth 25;

      -- miter line join (left)
        beginPath();
        moveTo(99, 150);
        lineTo(149, 50);
        lineTo(199, 150);
        lineJoin "miter";
        stroke();

      -- round line join (middle)
        beginPath();
        moveTo(239, 150);
        lineTo(289, 50);
        lineTo(339, 150);
        lineJoin "round";
        stroke();

      -- bevel line join (right)
        beginPath();
        moveTo(379, 150);
        lineTo(429, 50);
        lineTo(479, 150);
        lineJoin "bevel";
        stroke();
 

    wiki $ snapShot context "images/Line_Join.png"
    wiki $ close context
