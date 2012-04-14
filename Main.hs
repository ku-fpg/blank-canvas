import Graphics.Blank


main = blankCanvas 3000 $ \ canvas -> loop canvas (0 :: Float)
 where
--    loop canvas n | n > 2 = wait canvas
    loop canvas n = do
        send canvas $
                [ clearRect (0,0, width canvas, height canvas)
                , beginPath
                ] ++
                concat [
                [ save
                , translate (fromIntegral x * 80,height canvas / 2)
                , rotate (pi * n)
                , beginPath
                , moveTo(-100,-100)
                , lineTo(100,100)
                , lineWidth (fromIntegral x)
                , strokeStyle $ rgba (x * 20,255 - x * 20,128,255) -- fromIntegral x / 10,1 - fromIntegral x / 10,0.5,1)
                , stroke
                , restore
                ] | x <- [1..10]
                ]
        loop canvas (n + 0.01) -- if n > 0.99 then 0 else n + 0.01)

