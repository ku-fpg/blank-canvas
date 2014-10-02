{-# LANGUAGE OverloadedStrings #-}
module Text_Align where

import Graphics.Blank
import Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do
        let x = width context / 2
        let y = height context / 2
        font "30pt Calibri"
        textAlign "center"
        fillStyle "blue"
        fillText("Hello World!", x, y)


    wiki $ snapShot context "images/Text_Align.png"
    wiki $ close context
