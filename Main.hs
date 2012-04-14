import Graphics.Blank


main = blankCanvas 3000 $ \ canvas -> loop canvas (0 :: Float)
 where
--    loop canvas n | n > 2 = wait canvas
    loop canvas n = do
        send canvas
                [ clearRect (0,0, width canvas, height canvas)
                , beginPath
                , save
                , translate (width canvas / 2,height canvas / 2)
                , rotate (pi * n)
                , moveTo(-100,-100)
                , lineTo(100,100)
                , lineWidth 5
                , strokeStyle "#ff0000"
                , stroke
                , restore
                ]
        loop canvas (n + 0.01) -- if n > 0.99 then 0 else n + 0.01)

