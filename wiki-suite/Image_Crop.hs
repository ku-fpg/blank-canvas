{-# LANGUAGE OverloadedStrings #-}
module Image_Crop where

import Graphics.Blank
import Paths_wiki_suite
import Wiki -- (578,200)

main :: IO ()
main = do
    dat <- getDataDir
    blankCanvas 3000 { root = dat } $ \ context -> do
        send context $ do
            img <- newImage "/images/Haskell.jpg"
            drawImage(img,[150,0,150,150,50,50,150,200])
        
        
        wiki $ snapShot context "images/Image_Crop.png"
        wiki $ close context
