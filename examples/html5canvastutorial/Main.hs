{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Applicative

import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

import           Graphics.Blank
import qualified Graphics.Blank.Style as Style

import           Paths_blank_canvas_examples

main :: IO ()
main = do
 dat <- getDataDir        
 blankCanvas 3000 { events = ["mousedown"], 
                         debug = True,
                          static = ["images/" ++ img
                                   | img <- ["fan.jpg", "princess.jpg"]
                                   ]
                        , root = dat ++ "/static"
                        } $ \ canvas -> do
  sequence_ [ -- blank the screeen

              do send canvas $ do
                      let (w,h) = size canvas
                      clearRect (0,0,w,h)
                      beginPath()
                      globalAlpha 1.0 -- reset this global property

                 -- run this example
                 send canvas $ do
                      save()

                 example canvas

                 send canvas $ do
                      restore()

                 -- draw the watermark in corner
                 send canvas $ message canvas name
                 
                 -- wait for a mouse press
                 wait canvas 

            | (example,name) <- drop 15 $ cycle (map wrap examples ++ io_examples)
            ]
    where wrap (example,name) = (send <*> example, name)

examples :: [(DeviceContext -> Canvas (), Text)]
examples =
	 -- Lines
        [ (example_1_2_1,"1.2.1 Line")
        , (example_1_2_2,"1.2.2 Line Width")
        , (example_1_2_3,"1.2.3 Line Color")
        , (example_1_2_4,"1.2.4 Line Cap")
	-- Curves
        , (example_1_3_1,"1.3.1 Arc")
        , (example_1_3_2,"1.3.2 Quadratic Curve")
        , (example_1_3_3,"1.3.3 Bezier Curve")
	-- Paths
        , (example_1_4_1,"1.4.1 Path")
        , (example_1_4_2,"1.4.2 Line Join")
        , (example_1_4_3,"1.4.3 Rounded Corners")
	-- Shapes
        , (example_1_5_1,"1.5.1 Custom Shape")
        , (example_1_5_2,"1.5.2 Rectangle")
        , (example_1_5_3,"1.5.3 Circle")
        , (example_1_5_4,"1.5.4 Semicircle")
	-- Fill Styles
        , (example_1_6_1,"1.6.1 Shape Fill")
        , (example_1_6_2,"1.6.2 Linear Gradient")
        , (example_1_6_3,"1.6.3 Radial Gradient")
        , (example_1_6_4,"1.6.4 Pattern")
	-- Images
        , (example_1_7_1,"1.7.1 Image")
        , (example_1_7_2,"1.7.2 Image Size")
        , (example_1_7_3,"1.7.3 Image Crop")
        , (example_1_7_4,"1.7.4 Image Loader")
	-- Text
        , (example_1_8_1,"1.8.1 Text Font & Size")
        , (example_1_8_2,"1.8.2 Text Color")
        , (example_1_8_3,"1.8.3 Text Stroke")
        , (example_1_8_4,"1.8.4 Text Align")
        , (example_1_8_5,"1.8.5 Text Baseline")
        , (example_1_8_6,"1.8.6 Text Metrics") 
        , (example_1_8_7,"1.8.7 Text Wrap")
        -- Transformations 2.1
        , (example_2_1_1,"2.1.1 Translate Transform")
        -- Composites 2.2
        , (example_2_2_1,"2.2.1 Shadow")
        , (example_2_2_2,"2.2.2 Global Alpha")
        , (example_2_2_3,"2.2.3 Clipping Region")
        , (example_2_2_4,"2.2.4 Global Composite Operations")
        -- Image Data & URLs 2.3
--        , (example_2_3_1,"2.3.1 Image Data")
--        , (example_2_3_2,"2.3.2 Invert Image Colors")
--        , (example_2_3_3,"2.3.3 Grayscale Image Colors")
        -- Animation 2.4
	-- Mouse Detection 2.5
        ]

io_examples :: [(DeviceContext -> IO (), Text)]
io_examples =
        [ (example_2_3_4,"2.3.4 Get Image Data URL")
        , (example_2_3_5,"2.3.5 Load Image Data URL")
        ]

-- Examples taken from http://www.html5canvastutorials.com/tutorials/html5-canvas-tutorials-introduction/

{- For example, here is the JavaScript for 1.2.1
          moveTo(100, 150);
          lineTo(450, 50);
          stroke();
-}
example_1_2_1, example_1_2_2, example_1_2_3, example_1_2_4,
    example_1_3_1, example_1_3_2, example_1_3_3,
    example_1_4_1, example_1_4_2, example_1_4_3,
    example_1_5_1, example_1_5_2, example_1_5_3, example_1_5_4,
    example_1_6_1, example_1_6_2, example_1_6_3, example_1_6_4,
    example_1_7_1, example_1_7_2, example_1_7_3, example_1_7_4,
    example_1_8_1, example_1_8_2, example_1_8_3, example_1_8_4, example_1_8_5, example_1_8_6, example_1_8_7,
    example_2_1_1,
    example_2_2_1, example_2_2_2, example_2_2_3, example_2_2_4
  :: Image image => image -> Canvas ()

example_1_2_1 _ = do
        moveTo(100,150)
        lineTo(450,50)
        stroke()

example_1_2_2 _ = do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 15
        stroke()

example_1_2_3 _ = do
        moveTo(100,150)
        lineTo(450,50)
        lineWidth 5
        strokeStyle "#ff0000"
        stroke()

example_1_2_4 canvas = do
        let (w,h) = size canvas

        sequence_
           [ do beginPath()
                moveTo(200, h / 2 + n)
                lineTo(w - 200, h / 2 + n)
                lineWidth 20
                strokeStyle "#0000ff"
                lineCap cap
                stroke()
           | (cap,n) <- zip ["butt","round","square"] [-50,0,50]
           ]

example_1_3_1 canvas = do
        let (w,h) = size canvas
        let centerX = w / 2;
        let centerY = h / 2;
        let radius = 75;
        let startingAngle = 1.1 * pi
        let endingAngle = 1.9 * pi
        let counterclockwise = False
        arc(centerX, centerY, radius, startingAngle, endingAngle, counterclockwise)
        lineWidth 15
        strokeStyle "black"
        stroke()

example_1_3_2 _ = do
	beginPath()
	moveTo(188, 150)
        quadraticCurveTo(288, 0, 388, 150)
        lineWidth 10
        -- line color
        strokeStyle "black"
        stroke()

example_1_3_3 _ = do
	beginPath()
	moveTo(188, 150)
	bezierCurveTo(140, 10, 388, 10, 388, 170)
        lineWidth 10
        -- line color
        strokeStyle "black"
        stroke()

example_1_4_1 _ = do

        beginPath()
        moveTo(100, 20)
	-- line 1
        lineTo(200, 160)
        -- quadratic curve
        quadraticCurveTo(230, 200, 250, 120)
        -- bezier curve
        bezierCurveTo(290, -40, 300, 200, 400, 150)
        -- line 2
        lineTo(500, 90)
        lineWidth 5
        strokeStyle "blue"
        stroke()


example_1_4_2 _ = do
        lineWidth 25;

      -- miter line join (left)
        beginPath();
        moveTo(99, 150);
        lineTo(149, 50);
        lineTo(199, 150);
        lineJoin "miter";
        stroke();

      -- round line join (middle)
        beginPath();
        moveTo(239, 150);
        lineTo(289, 50);
        lineTo(339, 150);
        lineJoin "round";
        stroke();

      -- bevel line join (right)
        beginPath();
        moveTo(379, 150);
        lineTo(429, 50);
        lineTo(479, 150);
        lineJoin "bevel";
        stroke();
 
example_1_4_3 _ = do
        lineWidth 25;

        let rectWidth = 200;
        let rectHeight = 100;
        let rectX = 189;
        let rectY = 50;
        let cornerRadius = 50;

        beginPath();
        moveTo(rectX, rectY);
        lineTo(rectX + rectWidth - cornerRadius, rectY);
        arcTo(rectX + rectWidth, rectY, rectX + rectWidth, rectY + cornerRadius, cornerRadius);
        lineTo(rectX + rectWidth, rectY + rectHeight);
        lineWidth 5;
        stroke();

example_1_5_1 _ = do
        beginPath();
        moveTo(170, 80);
        bezierCurveTo(130, 100, 130, 150, 230, 150);
        bezierCurveTo(250, 180, 320, 180, 340, 150);
        bezierCurveTo(420, 150, 420, 120, 390, 100);
        bezierCurveTo(430, 40, 370, 30, 340, 50);
        bezierCurveTo(320, 5, 250, 20, 250, 50);
        bezierCurveTo(200, 5, 150, 20, 170, 80);
      -- complete custom shape
        closePath();
        lineWidth 5;
        strokeStyle "blue";
        stroke();

example_1_5_2 _ = do
        beginPath();
        rect(188, 50, 200, 100);
        fillStyle "yellow";
        fill();
        lineWidth 7;
        strokeStyle "black";
        stroke();
      
example_1_5_3 canvas = do
        let (w,h) = size canvas
        let centerX = w / 2
        let centerY = h / 2
        let radius = 70

        beginPath()
        arc(centerX, centerY, radius, 0, 2 * pi, False)
        fillStyle "#8ED6FF"
        fill()
        lineWidth  5
        strokeStyle "black"
        stroke()

example_1_5_4 _ = do
        beginPath();
        arc(288, 75, 70, 0, pi, False);
        closePath();
        lineWidth 5;
        fillStyle "red";
        fill();
        strokeStyle "#550000";
        stroke();

example_1_6_1 _ = do
        beginPath();
        moveTo(170, 80);
        bezierCurveTo(130, 100, 130, 150, 230, 150);
        bezierCurveTo(250, 180, 320, 180, 340, 150);
        bezierCurveTo(420, 150, 420, 120, 390, 100);
        bezierCurveTo(430, 40, 370, 30, 340, 50);
        bezierCurveTo(320, 5, 250, 20, 250, 50);
        bezierCurveTo(200, 5, 150, 20, 170, 80);

      -- complete custom shape
        closePath();
        lineWidth 5;
        fillStyle "#8ED6FF";
        fill();
        strokeStyle "blue";
        stroke();

example_1_6_2 canvas = do
        let (w,h) = size canvas
        rect(0, 0, w, h)
        grd <- createLinearGradient(0, 0, w, h)
        -- light blue
        grd # addColorStop(0, "#8ED6FF")
        -- dark blue
        grd # addColorStop(1, "#004CB3")
        Style.fillStyle grd;
        fill();

example_1_6_3 canvas = do
        let (w,h) = size canvas
        rect(0, 0, w, h)
        grd <- createRadialGradient (238, 50, 10, 238, 50, 300)
        -- light blue
        grd # addColorStop(0, "#8ED6FF")
        -- dark blue
        grd # addColorStop(1, "#004CB3")
        Style.fillStyle grd;
        fill();

example_1_6_4 canvas = do
        let (w,h) = size canvas
        imageObj <- newImage "/images/fan.jpg"
        pattern <- createPattern (imageObj,"repeat")
        rect(0, 0, w, h);
        Style.fillStyle pattern;
        fill();

example_1_7_1 _ = do
        img <- newImage "/images/princess.jpg"
        drawImage(img,[69,50])

example_1_7_2 _ = do
        img <- newImage "/images/princess.jpg"
        drawImage(img,[69,50,97,129])

example_1_7_3 _ = do
        img <- newImage "/images/princess.jpg"
        drawImage(img,[400,200,300,400,100,100,150,200])

example_1_7_4 _ = do
        img1 <- newImage "/images/princess.jpg"
        img2 <- newImage "/images/fan.jpg"
        drawImage(img1,[69,50,97,129])
        drawImage(img2,[400,50])

example_1_8_1 _ = do
        font "40pt Calibri"
        fillText("Hello World!", 150, 100)

example_1_8_2 _ = do
        font "40pt Calibri"
        fillStyle "#0000ff"
        fillText("Hello World!", 150, 100)

example_1_8_3 _ = do
        font "60pt Calibri"
        lineWidth 3
        strokeStyle "blue"
        strokeText("Hello World!", 80, 110)

example_1_8_4 canvas = do
        let (w,h) = size canvas
        let x = w / 2
        let y = h / 2
        font "30pt Calibri"
        textAlign "center"
        fillStyle "blue"
        fillText("Hello World!", x, y)

example_1_8_5 canvas = do
        let (w,h) = size canvas
        let x = w / 2
        let y = h / 2
        font "30pt Calibri"
        textAlign "center"
        textBaseline "middle"
        fillStyle "blue"
        fillText("Hello World!", x, y)

example_1_8_6 canvas = do
        let (w,h) = size canvas
        let x = w / 2
        let y = h / 2 - 10;
        let text = "Hello World!"
        font "30pt Calibri"
        textAlign "center"
        fillStyle "blue"
        fillText(text, x, y)

        TextMetrics w' <- measureText text
        font "20pt Calibri"
        textAlign "center"
        fillStyle "#555"
        fillText("(" <> Text.pack (show w') <> "px wide)", x, y + 40)

example_1_8_7 canvas = do
        let w = width canvas
        font "lighter 16pt Calibri"
        fillStyle "#000"
        let maxWidth = w / 3
        wrapText 0 (Text.words message') ((w - maxWidth) / 2) 60 maxWidth 25
    where

        message' = "All the world's a stage, and all the men and women merely players. " <>
                  "They have their exits and their entrances; And one man in his time plays many parts."

        wrapText _  []   _ _ _        _          = return ()
        wrapText wc text x y maxWidth lineHeight = do
             TextMetrics testWidth <- measureText $ Text.unwords $ take (wc+1) $ text
             if (testWidth > maxWidth && wc > 0) || length text <= wc
             then do fillText(Text.unwords $ take wc $ text,x,y)
                     wrapText 0      (drop wc text) x (y + lineHeight) maxWidth lineHeight
             else do wrapText (wc+1) text           x y                maxWidth lineHeight


example_2_1_1 canvas = do
        let (w,h) = size canvas
        let rectWidth = 150;
        let rectHeight = 75;
        translate(w / 2, h / 2);
        fillStyle "blue";
        fillRect(rectWidth / (-2), rectHeight / (-2), rectWidth, rectHeight);

example_2_2_1 _ = do
        rect(188, 40, 200, 100);
        fillStyle "red";
        shadowColor "#999";
        shadowBlur 20;
        shadowOffsetX 15;
        shadowOffsetY 15;
        fill()

example_2_2_2 _ = do
      -- draw blue rectangle
      beginPath();
      rect(200, 20, 100, 100);
      fillStyle "blue";
      fill();

      -- draw transparent red circle
      globalAlpha 0.5;
      beginPath();
      arc(320, 120, 60, 0, 2 * pi, False);
      fillStyle "red";
      fill();

example_2_2_3 canvas = do
      let (w,h) = size canvas
      let x = w / 2;
      let y = h / 2;
      let radius = 75;
      let offset = 50;

      {-
       * save() allows us to save the canvas context before
       * defining the clipping region so that we can return
       * to the default state later on
       -}
      save();
      beginPath();
      arc(x, y, radius, 0, 2 * pi, False);
      clip();

      -- draw blue circle inside clipping region
      beginPath();
      arc(x - offset, y - offset, radius, 0, 2 * pi, False);
      fillStyle "blue";
      fill();

      -- draw yellow circle inside clipping region
      beginPath();
      arc(x + offset, y, radius, 0, 2 * pi, False);
      fillStyle "yellow";
      fill();

      -- draw red circle inside clipping region
      beginPath();
      arc(x, y + offset, radius, 0, 2 * pi, False);
      fillStyle "red";
      fill();

      {-
       * restore() restores the canvas context to its original state
       * before we defined the clipping region
       -}
      restore();
      beginPath();
      arc(x, y, radius, 0, 2 * pi, False);
      lineWidth 10;
      strokeStyle "blue";
      stroke();

example_2_2_4 canvas = do
        let (w,h) = size canvas :: (Float, Float)
        tempCanvas <- newCanvas (round w,round h)
        console_log tempCanvas
        let (w',h') = size tempCanvas
        console_log $ Text.pack $ show $ (w',h')

        sync
        
        let squareWidth = 55;
        let circleRadius = 35;
        let shapeOffset = 50;
        -- let operationOffset = 150;

        let compss = 
             [["source-atop", "source-in", "source-out", "source-over"]
             ,["destination-atop","destination-in","destination-out","destination-over"]
             ,["lighter","darker","xor","copy"]
             ]

        -- translate context to add 10px padding
        translate(10, 10);


        sequence_ [
             do

                -- clear temp context
                with tempCanvas $ do
                        save();

                        clearRect(0, 0, w', h');
                        -- draw rectangle (destination)
                        beginPath();
                        rect(0, 0, squareWidth, squareWidth);
                        fillStyle "blue";
                        fill();

                        -- set global composite
                        globalCompositeOperation thisOperation;

                        -- draw circle (source)
                        beginPath();
                        arc(shapeOffset, shapeOffset, circleRadius, 0, 2 * pi, False);
                        fillStyle "red";
                        fill();

                        restore();

                        font "10pt Verdana";
                        fillStyle "black";
                        fillText(thisOperation, 0, squareWidth + 45);

                drawImage(tempCanvas, [x * 125, y * 125]);


              | (comps,y)         <- compss `zip` [0..]
              , (thisOperation,x) <- comps  `zip` [0..]
              ]

example_2_3_4, example_2_3_5 :: DeviceContext -> IO ()

example_2_3_4 canvas = do
   url <- send canvas $ do
        beginPath();
        moveTo(170, 80);
        bezierCurveTo(130, 100, 130, 150, 230, 150);
        bezierCurveTo(250, 180, 320, 180, 340, 150);
        bezierCurveTo(420, 150, 420, 120, 390, 100);
        bezierCurveTo(430, 40, 370, 30, 340, 50);
        bezierCurveTo(320, 5, 250, 20, 250, 50);
        bezierCurveTo(200, 5, 150, 20, 170, 80);
      -- complete custom shape
        closePath();
        lineWidth 5;
        strokeStyle "blue";
        stroke();
        toDataURL();

   send canvas $ do
        font "18pt Calibri"
        fillText(Text.pack $ show $ Text.take 50 $ url, 10, 300)

example_2_3_5 canvas = do
   fileName <- getDataFileName "static/data/dataURL.txt"
   url <- Text.IO.readFile fileName
   send canvas $ do
           img <- newImage url
           drawImage (img,[0,0])

-- example_last :: Canvas ()
-- example_last = do
--         todo  -- marker for the scanning sof the examples

---------------------------------------------------------------------------

-- todo :: Canvas ()
-- todo = do
--         font "40pt Calibri"
--         fillText("(TODO)", 150, 100)

-- Small "watermark-like text in the bottom corner"
message :: DeviceContext -> Text -> Canvas ()
message canvas msg = do
        save()
        let h = height canvas
        font "30pt Calibri"
        textAlign "left"
        fillStyle "#8090a0"
        fillText(msg, 10, h - 10)
        restore()

size :: (Image image, Num a, Num b) => image -> (a, b)
size image = (width image, height image)