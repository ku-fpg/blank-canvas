{-# LANGUAGE OverloadedStrings #-}
module Clipping_Region where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
      let x = width context / 2;
      let y = height context / 2;
      let radius = 75;
      let offset = 50;

      {-
       * save() allows us to save the canvas context before
       * defining the clipping region so that we can return
       * to the default state later on
       -}
      save();
      beginPath();
      arc(x, y, radius, 0, 2 * pi, False);
      clip();

      -- draw blue circle inside clipping region
      beginPath();
      arc(x - offset, y - offset, radius, 0, 2 * pi, False);
      fillStyle "blue";
      fill();

      -- draw yellow circle inside clipping region
      beginPath();
      arc(x + offset, y, radius, 0, 2 * pi, False);
      fillStyle "yellow";
      fill();

      -- draw red circle inside clipping region
      beginPath();
      arc(x, y + offset, radius, 0, 2 * pi, False);
      fillStyle "red";
      fill();

      {-
       * restore() restores the canvas context to its original state
       * before we defined the clipping region
       -}
      restore();
      beginPath();
      arc(x, y, radius, 0, 2 * pi, False);
      lineWidth 10;
      strokeStyle "blue";
      stroke();

    wiki $ snapShot context "images/Clipping_Region.png"
    wiki $ close context
