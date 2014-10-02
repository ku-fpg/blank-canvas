{-# LANGUAGE OverloadedStrings #-}
module Custom_Transform where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let rectWidth = 150;
        let rectHeight = 75;

        -- translation matrix:
        --  1  0  tx
        --  0  1  ty
        --  0  0  1
        let tx = width context / 2;
        let ty = height context / 2;

        -- apply custom transform
        transform(1, 0, 0, 1, tx, ty);

        fillStyle "blue";
        fillRect(rectWidth / (-2), rectHeight / (-2), rectWidth, rectHeight);


    wiki $ snapShot context "images/Custom_Transform.png"
    wiki $ close context
