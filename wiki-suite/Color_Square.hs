{-# LANGUAGE OverloadedStrings #-}
module Color_Square where

import qualified Data.Vector.Unboxed as V
import           Graphics.Blank
import           Wiki -- (300,300)

main :: IO ()
main = blankCanvas 3000 $ \ context -> do
    let v = V.fromList
          $ concat
          $ [ [r,g,0,255]
            | r <- [0..255]
            , g <- [0..255]
            ]

    send context $ do
        putImageData (ImageData 256 256 v, [22,22])

    wiki $ snapShot context "images/Color_Square.png"
    wiki $ close context
    
