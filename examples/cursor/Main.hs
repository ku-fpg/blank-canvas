{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Functor (void)
import Data.Key (forWithKey_)
import Data.List (genericLength)
import Data.Monoid ((<>))
import Data.Text (Text)

import Graphics.Blank

import Paths_blank_canvas_examples

main :: IO ()
main = do
    dat <- getDataDir
    blankCanvas 3000 { root = dat, events = ["mousedown"] } $ \context -> do
        let w, h, delta :: Double
            w     = width context
            h     = height context
            delta = h / genericLength cursorValues
        
        send context $ do
            void $ newImage(cursorImage);
            font("12pt Calibri");
            forWithKey_ cursorValues $ \i text -> do
                let isEven :: Bool
                    isEven = i `mod` 2 == 0
                    
                    i' :: Double
                    i' = fromIntegral i
                
                TextMetrics textWidth <- measureText(text);
                fillStyle(if isEven then "black" else "white");
                fillRect(0, i'*delta, w, (i'+1)*delta);
                fillStyle(if isEven then "white" else "black");
                fillText(text, w/2 - textWidth/2, i'*delta+17);
            
        let pointToCursor :: Double -> Text
            pointToCursor y = let i = floor $ y / delta
                              in if i < 0 || i >= length cursorValues
                                    then "default"
                                    else cursorValues !! i
        
        let loop :: IO ()
            loop = do
                event <- wait context
                case ePageXY event of
                     Nothing     -> loop
                     Just (_, y) -> do
                         let cur = pointToCursor y
                         send context $ cursor(cur);
                         loop
        
        loop

cursorImage :: Text
cursorImage = "/images/cursor.png"

cursorValues :: [Text]
cursorValues = [ "auto"
               , "default"
               , "none"
               , "context-menu"
               , "help"
               , "pointer"
               , "progress"
               , "wait"
               , "cell"
               , "crosshair"
               , "text"
               , "vertical-text"
               , "alias"
               , "copy"
               , "move"
               , "no-drop"
               , "not-allowed"
               , "all-scroll"
               , "col-resize"
               , "row-resize"
               , "n-resize"
               , "e-resize"
               , "s-resize"
               , "w-resize"
               , "ne-resize"
               , "nw-resize"
               , "se-resize"
               , "sw-resize"
               , "ew-resize"
               , "ns-resize"
               , "nesw-resize"
               , "nwse-resize"
               , "zoom-in"
               , "zoom-out"
               , "grab"
               , "grabbing"
               , "url('" <> cursorImage <> "'), default"
               ]      