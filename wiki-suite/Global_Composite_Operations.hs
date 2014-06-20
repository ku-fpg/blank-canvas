{-# LANGUAGE OverloadedStrings #-}
module Global_Composite_Operations where

import Graphics.Blank
import qualified Data.Text as Text
import Wiki -- (578,430)


main = blankCanvas 3000 $ \ context -> do
    send context $ do
        
        tempCanvas <- newCanvas (round (width context),round (height context))
        console_log tempCanvas
        (w,h) <- return (round (width context),round (height context))
        console_log $ Text.pack $ show $ (w,h)
        
        let squareWidth = 55;
        let circleRadius = 35;
        let shapeOffset = 50;
        let operationOffset = 150;

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

                        clearRect(0, 0, width context, height context);
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

    wiki $ snapShot context "images/Global_Composite_Operations.png"
    wiki $ close context
