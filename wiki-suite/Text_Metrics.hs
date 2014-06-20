{-# LANGUAGE OverloadedStrings #-}
module Text_Metrics where

import Graphics.Blank
import qualified Data.Text as Text
import Data.Monoid
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do        
        let x = width context / 2
        let y = height context / 2 - 10;
        let text = "Hello World!"
        font "30pt Calibri"
        textAlign "center"
        fillStyle "blue"
        fillText(text, x, y)

        TextMetrics w <- measureText text
        font "20pt Calibri"
        textAlign "center"
        fillStyle "#555"
        fillText("(" <> Text.pack (show w) <> "px wide)", x, y + 40)


    wiki $ snapShot context "images/Text_Metrics.png"
    wiki $ close context
