{-# LANGUAGE OverloadedStrings #-}
module Rounded_Corners where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        
        lineWidth 25;

        let rectWidth = 200;
        let rectHeight = 100;
        let rectX = 189;
        let rectY = 50;
        let cornerRadius = 50;

        beginPath();
        moveTo(rectX, rectY);
        lineTo(rectX + rectWidth - cornerRadius, rectY);
        arcTo(rectX + rectWidth, rectY, rectX + rectWidth, rectY + cornerRadius, cornerRadius);
        lineTo(rectX + rectWidth, rectY + rectHeight);
        lineWidth 5;
        stroke();


    wiki $ snapShot context "images/Rounded_Corners.png"
    wiki $ close context
