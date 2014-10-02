{-# LANGUAGE OverloadedStrings #-}
module Grayscale where

import qualified Data.Vector.Unboxed as V
import           Graphics.Blank
import           Paths_wiki_suite
import           Wiki -- (578,400)

main :: IO ()
main = do
    dat <- getDataDir
    blankCanvas 3000 { root = dat } $ \ context -> do
        ImageData w h v <- send context $ do
            img <- newImage "/images/House.jpg"
            drawImage(img,[50,50])
            getImageData(50,50, width img, height img)
        
        let group4 (a:b:c:d:xs) = (a,b,c,d) : group4 xs
            group4 _            = []
        
        let v' = V.fromList
              $ concat
              $ [ let brightness = round
                                 ( 0.34 * fromIntegral r 
                                 + 0.5 * fromIntegral g
                                 + 0.16 * fromIntegral b :: Double)
                  in [brightness,brightness,brightness,a]	     
                | (r,g,b,a) <- group4 $ V.toList $ v
                ]
        
        send context $ do
            putImageData (ImageData w h v', [100,100])
        
        wiki $ snapShot context "images/Grayscale.png"
        wiki $ close context
