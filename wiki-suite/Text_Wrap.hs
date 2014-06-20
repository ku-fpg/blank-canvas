{-# LANGUAGE OverloadedStrings #-}
module Text_Wrap where

import Graphics.Blank
import qualified Data.Text as Text
import Data.Monoid
import Wiki -- (578,200)

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        font "lighter 16pt Calibri"
        fillStyle "#000"
        let maxWidth = 400
        wrapText 0 (Text.words message) ((width context - maxWidth) / 2) 60 maxWidth 25
    wiki $ snapShot context "images/Text_Wrap.png"
    wiki $ close context
    where

        message = "All the world's a stage, and all the men and women merely players. " <>
                  "They have their exits and their entrances; And one man in his time plays many parts."

        wrapText wc []   x y maxWidth lineHeight = return ()
        wrapText wc text x y maxWidth lineHeight = do
             TextMetrics testWidth <- measureText $ Text.unwords $ take (wc+1) $ text
             if (testWidth > maxWidth && wc > 0) || length text <= wc
             then do fillText(Text.unwords $ take wc $ text,x,y)
                     wrapText 0      (drop wc text) x (y + lineHeight) maxWidth lineHeight
             else do wrapText (wc+1) text           x y                maxWidth lineHeight



