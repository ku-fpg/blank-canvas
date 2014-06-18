### Background

**blank-canvas** is a Haskell binding to the complete HTML5 Canvas API. blank-canvas allows Haskell users to write, in Haskell, interactive images onto their web browsers. blank-canvas gives the users a single full-window canvas, and provides many well-documented functions for rendering images.

### First Example

````Haskell
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Blank                     -- import the blank canvas

main = blankCanvas 3000 $ \ context -> do -- start blank canvas on port 3000
        send context $ do                 -- send commands to this specific context
                moveTo(50,50)
                lineTo(200,100)
                lineWidth 10
                strokeStyle "red"
                stroke()                  -- this draws the ink into the canvas
````

Running this program, and going to <http://localhost:3000/> gives

![images/Red_Line.png](https://github.com/ku-fpg/blank-canvas/wiki/images/Red_Line.png)

For more details about this example, see [Red Line](https://github.com/ku-fpg/blank-canvas/wiki/Red%20Line).

### Documentation

| Link  | Notes |
|-------|-------|
| [Examples](https://github.com/ku-fpg/blank-canvas/wiki/Examples) | Various complete examples of using blank-canvas |
| [Installation](https://github.com/ku-fpg/blank-canvas/wiki/Installation) | How to install blank-canvas |
| [Hackage](https://hackage.haskell.org/package/blank-canvas) | Current release is 0.4.0 |
| [API](https://github.com/ku-fpg/blank-canvas/wiki/API) | Discussion of API, compared with the original JavaScript API |
| [Canvas Tutorial](https://github.com/ku-fpg/blank-canvas/wiki/Canvas%20Tutorial) | Transliterated from <http://www.html5canvastutorials.com/> into Haskell and blank-canvas, with kind permission of Eric Rowell, author of the JavaScript HTML5 Canvas Tutorial. |

#### Other Links

 * <http://www.html5canvastutorials.com/>
 * <http://www.webcodeapp.com/>
