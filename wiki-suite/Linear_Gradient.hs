{-# LANGUAGE OverloadedStrings #-}
module Linear_Gradient where

import           Graphics.Blank
import qualified Graphics.Blank.Style as Style
import           Wiki -- (578,200)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    send context $ do        
        rect(0, 0, width context, height context)
        grd <- createLinearGradient(0, 0, width context, height context)
        grd # do 
            -- light blue
            addColorStop(0, "#8ED6FF")
            -- dark blue
            addColorStop(1, "#004CB3")
        Style.fillStyle grd;
        fill();


    wiki $ snapShot context "images/Linear_Gradient.png"
    wiki $ close context
