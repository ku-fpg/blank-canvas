{-# LANGUAGE OverloadedStrings #-}
module Semicircle where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath();
        arc(288, 75, 70, 0, pi, False);
        closePath();
        lineWidth 5;
        fillStyle "red";
        fill();
        strokeStyle "#550000";
        stroke();


    wiki $ snapShot context "images/Semicircle.png"
    wiki $ close context
