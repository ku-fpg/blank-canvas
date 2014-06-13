module Main (main) where

import Graphics.Blank

main = blankCanvas 3000 $ \ cxt -> send cxt $ do
     (width,height) <- size
     console_log $ show $ (width,height)

     fillStyle "black"
     textAlign "center"   
     sequence_ [ 
          do save()
             translate (x * width/4,(y+1) * height/16)
             let p' = round (p * 1/z)
             font ("lighter " ++ show p' ++ "pt " ++ "Chalkduster") --  Calibri")
             scale (z,z)
             fillText("Hello World! (" ++ show p' ++ ")", 0, 0)
             restore()
          | (x,p) <- [1..3] `zip` [9,18,36]
          , (y,z) <- [1..3] `zip` [0.5,1,2]
          ]
     drawImage(top, [0,0,width,height/2,0,height/2,width/2,height/4])
