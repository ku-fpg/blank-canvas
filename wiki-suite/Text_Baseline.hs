{-# LANGUAGE OverloadedStrings #-}
module Text_Baseline where

import Graphics.Blank
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let x = width context / 2
        let y = height context / 2
        font "30pt Calibri"
        textAlign "center"
        textBaseline "middle"
        fillStyle "blue"
        fillText("Hello World!", x, y)


    wiki $ snapShot context "images/Text_Baseline.png"
    wiki $ close context
