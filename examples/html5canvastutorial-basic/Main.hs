module Main (main) where

import Graphics.Blank

main = blankCanvas 3000 $ \ canvas ->
  sequence_ [ -- blank the screeen
              do send canvas $ do
                      (width,height) <- size
                      clearRect (0,0,width,height)
                      beginPath

                 -- run this example
                 send canvas $ do
                      save
                      example
                      restore

                 -- wait for a mouse press
                 send canvas $ readEvent MouseDown

            | example <- cycle examples
            ]


examples =
        [ example_1_2_1
        , example_1_2_2
        , example_1_2_3
        , example_1_2_4
        ]

-- Examples taken from http://www.html5canvastutorials.com/tutorials/html5-canvas-tutorials-introduction/

{- Here is the JavaScript:
        context.moveTo(100, 150);
        context.lineTo(450, 50);
        context.stroke();
-}
example_1_2_1 = do
        moveTo(100,150)
        lineTo(450,50)
        stroke

example_1_2_2 = do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 15
        stroke

example_1_2_3 = do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 5
        strokeStyle "#ff0000"
        stroke

example_1_2_4 = do
        (width,height) <- size

        sequence_
           [ do beginPath
                moveTo(200, height / 2 + n)
                lineTo(width - 200, height / 2 + n)
                lineWidth 20
                strokeStyle "#0000ff"
                lineCap cap
                stroke
           | (cap,n) <- zip ["butt","round","square"] [-50,0,50]
           ]

{-
html5canvastutorials
html5canvastutorials
        save
                translate (width / 2,height / 2)
                rotate (pi * n)
                beginPath
                moveTo(-100,-100)
                lineTo(-100,100)
                lineTo(100,100)
                lineTo(100,-100)
                closePath
                lineWidth 10
                strokeStyle $ rgba (100,200,128,255)
                stroke
                restore

        loop canvas (n + 0.01)

-}