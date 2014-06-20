{-# LANGUAGE OverloadedStrings #-}
module HPCTEST where

import Graphics.Blank
import Wiki -- (578,200)
import Trace.Hpc.Reflect

main = blankCanvas 3000 $ \ context -> do
    send context $ do
        beginPath()
        moveTo(100,150)
        lineTo(450,50)
        stroke()
    tix <- examineTix
    print tix

--    wiki $ snapShot context "images/Line.png"
--    wiki $ close context
