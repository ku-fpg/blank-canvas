{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import           Data.Text (Text)
import           Graphics.Blank
import           Prelude hiding ((++))

(++) :: Text -> Text -> Text
(++) = T.append
infixr 5 ++

imgPath :: Text
imgPath = "/images/fan.jpg"

main :: IO ()
main = blankCanvas 3000 $ \ ctx -> send ctx $ do
     let (w, h) = (width ctx, height ctx)
     console_log . T.pack . show $ (w, h)

     fillStyle "black"
     textAlign "center"   
     sequence_ [ 
          do save()
             translate (x * w/4,(y+1) * h/16)
             let p' = round (p * 1/z) :: Int
             font ("lighter " ++ (T.pack $ show p') ++ "pt " ++ "Chalkduster") --  Calibri")
             scale (z,z)
             fillText("Hello World! (" ++ (T.pack $ show p') ++ ")", 0, 0)
             restore()
          | (x,p) <- [1..3] `zip` [9,18,36]
          , (y,z) <- [1..3] `zip` [0.5,1,2]
          ]
     top <- newImage imgPath
     drawImage(top, [0,0,w,h/2,0,h/2,w/2,h/4])
