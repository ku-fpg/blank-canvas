{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Graphics.Blank.Canvas
import Graphics.Blank.JavaScript

instance Show Method where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "arc(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (ArcTo (a1,a2,a3,a4,a5)) = "arcTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ ")"
  show BeginPath = "beginPath()"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "bezierCurveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (DrawImage (a1,a2)) = "drawImage(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (ClearRect (a1,a2,a3,a4)) = "clearRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show Clip = "clip()"
  show ClosePath = "closePath()"
  show Fill = "fill()"
  show (FillRect (a1,a2,a3,a4)) = "fillRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (FillStyle (a1)) = "fillStyle = (" ++ showJS a1 ++ ")"
  show (FillText (a1,a2,a3)) = "fillText(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ ")"
  show (Font (a1)) = "font = (" ++ showJS a1 ++ ")"
  show (GlobalAlpha (a1)) = "globalAlpha = (" ++ showJS a1 ++ ")"
  show (GlobalCompositeOperation (a1)) = "globalCompositeOperation(" ++ showJS a1 ++ ")"
  show (LineCap (a1)) = "lineCap = (" ++ showJS a1 ++ ")"
  show (LineJoin (a1)) = "lineJoin = (" ++ showJS a1 ++ ")"
  show (LineTo (a1,a2)) = "lineTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (LineWidth (a1)) = "lineWidth = (" ++ showJS a1 ++ ")"
  show (MiterLimit (a1)) = "miterLimit = (" ++ showJS a1 ++ ")"
  show (MoveTo (a1,a2)) = "moveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (PutImageData (a1,a2)) = "putImageData(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (QuadraticCurveTo (a1,a2,a3,a4)) = "quadraticCurveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (Rect (a1,a2,a3,a4)) = "rect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show Restore = "restore()"
  show (Rotate (a1)) = "rotate(" ++ showJS a1 ++ ")"
  show (Scale (a1,a2)) = "scale(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show Save = "save()"
  show (SetTransform (a1,a2,a3,a4,a5,a6)) = "setTransform(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show Stroke = "stroke()"
  show (StrokeRect (a1,a2,a3,a4)) = "strokeRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (StrokeText (a1,a2,a3)) = "strokeText(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ ")"
  show (StrokeStyle (a1)) = "strokeStyle = (" ++ showJS a1 ++ ")"
  show (ShadowBlur (a1)) = "shadowBlur = (" ++ showJS a1 ++ ")"
  show (ShadowColor (a1)) = "shadowColor = (" ++ showJS a1 ++ ")"
  show (ShadowOffsetX (a1)) = "shadowOffsetX = (" ++ showJS a1 ++ ")"
  show (ShadowOffsetY (a1)) = "shadowOffsetY = (" ++ showJS a1 ++ ")"
  show (TextAlign (a1)) = "textAlign = (" ++ showJS a1 ++ ")"
  show (TextBaseline (a1)) = "textBaseline = (" ++ showJS a1 ++ ")"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "transform(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (Translate (a1,a2)) = "translate(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"

-- DSL

arc :: (Float,Float,Float,Float,Float,Bool) -> Canvas ()
arc = Method . Arc

arcTo :: (Float,Float,Float,Float,Float) -> Canvas ()
arcTo = Method . ArcTo

beginPath :: () -> Canvas ()
beginPath () = Method BeginPath

bezierCurveTo :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
bezierCurveTo = Method . BezierCurveTo

drawImage :: Image image => (image,[Float]) -> Canvas ()
drawImage = Method . DrawImage

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Method . ClearRect

clip :: () -> Canvas ()
clip () = Method Clip

closePath :: () -> Canvas ()
closePath () = Method ClosePath

fill :: () -> Canvas ()
fill () = Method Fill

fillRect :: (Float,Float,Float,Float) -> Canvas ()
fillRect = Method . FillRect

fillStyle :: Style style => style -> Canvas ()
fillStyle = Method . FillStyle

fillText :: (String,Float,Float) -> Canvas ()
fillText = Method . FillText

font :: String -> Canvas ()
font = Method . Font

globalAlpha :: Float -> Canvas ()
globalAlpha = Method . GlobalAlpha

globalCompositeOperation :: String -> Canvas ()
globalCompositeOperation = Method . GlobalCompositeOperation

lineCap :: String -> Canvas ()
lineCap = Method . LineCap

lineJoin :: String -> Canvas ()
lineJoin = Method . LineJoin

lineTo :: (Float,Float) -> Canvas ()
lineTo = Method . LineTo

lineWidth :: Float -> Canvas ()
lineWidth = Method . LineWidth

miterLimit :: Float -> Canvas ()
miterLimit = Method . MiterLimit

moveTo :: (Float,Float) -> Canvas ()
moveTo = Method . MoveTo

putImageData :: (ImageData,[Float]) -> Canvas ()
putImageData = Method . PutImageData

quadraticCurveTo :: (Float,Float,Float,Float) -> Canvas ()
quadraticCurveTo = Method . QuadraticCurveTo

rect :: (Float,Float,Float,Float) -> Canvas ()
rect = Method . Rect

restore :: () -> Canvas ()
restore () = Method Restore

rotate :: Float -> Canvas ()
rotate = Method . Rotate

scale :: (Float,Float) -> Canvas ()
scale = Method . Scale

save :: () -> Canvas ()
save () = Method Save

setTransform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
setTransform = Method . SetTransform

stroke :: () -> Canvas ()
stroke () = Method Stroke

strokeRect :: (Float,Float,Float,Float) -> Canvas ()
strokeRect = Method . StrokeRect

strokeText :: (String,Float,Float) -> Canvas ()
strokeText = Method . StrokeText

strokeStyle :: String -> Canvas ()
strokeStyle = Method . StrokeStyle

shadowBlur :: Float -> Canvas ()
shadowBlur = Method . ShadowBlur

shadowColor :: String -> Canvas ()
shadowColor = Method . ShadowColor

shadowOffsetX :: Float -> Canvas ()
shadowOffsetX = Method . ShadowOffsetX

shadowOffsetY :: Float -> Canvas ()
shadowOffsetY = Method . ShadowOffsetY

textAlign :: String -> Canvas ()
textAlign = Method . TextAlign

textBaseline :: String -> Canvas ()
textBaseline = Method . TextBaseline

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Method . Transform

translate :: (Float,Float) -> Canvas ()
translate = Method . Translate

