{-# LANGUAGE OverloadedStrings #-}
module Rectangle where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath();
        rect(188, 50, 200, 100);
        fillStyle "yellow";
        fill();
        lineWidth 7;
        strokeStyle "black";
        stroke();
      

    wiki $ snapShot context "images/Rectangle.png"
    wiki $ close context
