{-# LANGUAGE OverloadedStrings #-}
module Custom_Shape where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath();
        moveTo(170, 80);
        bezierCurveTo(130, 100, 130, 150, 230, 150);
        bezierCurveTo(250, 180, 320, 180, 340, 150);
        bezierCurveTo(420, 150, 420, 120, 390, 100);
        bezierCurveTo(430, 40, 370, 30, 340, 50);
        bezierCurveTo(320, 5, 250, 20, 250, 50);
        bezierCurveTo(200, 5, 150, 20, 170, 80);
      -- complete custom shape
        closePath();
        lineWidth 5;
        strokeStyle "blue";
        stroke();


    wiki $ snapShot context "images/Custom_Shape.png"
    wiki $ close context
