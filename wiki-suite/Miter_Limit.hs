{-# LANGUAGE OverloadedStrings #-}
module Miter_Limit where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
  send context $ do
    clearRect(0,0,150,150);
    -- Draw guides
    strokeStyle "#09f";
    lineWidth    2;
    strokeRect(-5,50,160,50);
 
    -- Set line styles
    strokeStyle "#000";
    lineWidth 10;
 
    -- check input
    miterLimit 5;

    -- Draw lines
    beginPath()
    moveTo(0,100)
    sequence_ [ lineTo((fromIntegral i ** 1.5)*2,75+(if i `mod` 2 == 0 then 25 else -25))
    	      | i <-[0..20]
	      ]
    stroke();

  wiki $ snapShot context "images/Miter_Limit.png"
  wiki $ close context
