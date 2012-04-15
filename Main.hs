import Graphics.Blank


main = blankCanvas 3000 $ \ canvas -> loop canvas (0 :: Float)
 where
--    loop canvas n | n > 2 = wait canvas
    loop canvas n = do

        send canvas $ do
                (width,height) <- size
                clearRect (0,0,width,height)
                beginPath
                sequence_ [ do
                  save
                  translate (fromIntegral x * 80,height / 2)
                  rotate (pi * n)
                  beginPath
                  moveTo(-100,-100)
                  lineTo(-100,100)
                  lineTo(100,100)
                  lineTo(100,-100)
                  closePath
                  lineWidth (fromIntegral x)
                  strokeStyle $ rgba (x * 20,255 - x * 20,128,255) -- fromIntegral x / 10,1 - fromIntegral x / 10,0.5,1)
                  stroke
                  restore | x <- [5] ] -- 1..10] ]

        res <- send canvas $ tryReadEvent MouseMove
        print res

        loop canvas (n + 0.01) -- if n > 0.99 then 0 else n + 0.01)

