{-# LANGUAGE OverloadedStrings #-}
module Global_Alpha where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
      -- draw blue rectangle
      beginPath();
      rect(200, 20, 100, 100);
      fillStyle "blue";
      fill();

      -- draw transparent red circle
      globalAlpha 0.5;
      beginPath();
      arc(320, 120, 60, 0, 2 * pi, False);
      fillStyle "red";
      fill();


    wiki $ snapShot context "images/Global_Alpha.png"
    wiki $ close context
