{-# LANGUAGE OverloadedStrings #-}
module Get_Image_Data_URL where

import Graphics.Blank
import Wiki -- (578,350)
import qualified Data.Text as Text

main = blankCanvas 3000 $ \ context -> do
    url <- send context $ do
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
        toDataURL();

    send context $ do
        font "18pt Calibri"
        fillText(Text.pack $ show $ Text.take 50 $ url, 10, 300)

    wiki $ snapShot context "images/Get_Image_Data_URL.png"
    wiki $ close context
