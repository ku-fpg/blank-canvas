{-# LANGUAGE OverloadedStrings #-}
module Pattern where

import Graphics.Blank
import qualified Graphics.Blank.Style as Style
import Wiki -- (578,200)

main = blankCanvas 3000 { static = ["images/Haskell.jpg"] } $ \ context -> do
    send context $ do
        imageObj <- newImage "/images/Haskell.jpg"
        pattern <- createPattern (imageObj,"repeat")
        rect(0, 0, width context, height context);
        Style.fillStyle pattern;
        fill();


    wiki $ snapShot context "images/Pattern.png"
    wiki $ close context
