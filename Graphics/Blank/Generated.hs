{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Graphics.Blank.Canvas

instance Show Command where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "c.arc(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (ArcTo (a1,a2,a3,a4,a5)) = "c.arcTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ ")"
  show BeginPath = "c.beginPath()"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "c.bezierCurveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (DrawImage (a1,a2)) = "c.drawImage(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (ClearRect (a1,a2,a3,a4)) = "c.clearRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show Clip = "c.clip()"
  show ClosePath = "c.closePath()"
  show Fill = "c.fill()"
  show (FillRect (a1,a2,a3,a4)) = "c.fillRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (FillStyle a) = "c.fillStyle = (" ++ showJS a ++ ")"
  show (FillText (a1,a2,a3)) = "c.fillText(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ ")"
  show (Font (a1)) = "c.font = (" ++ showJS a1 ++ ")"
  show (GlobalAlpha (a1)) = "c.globalAlpha = (" ++ showJS a1 ++ ")"
  show (GlobalCompositeOperation (a1)) = "c.globalCompositeOperation(" ++ showJS a1 ++ ")"
  show (LineCap (a1)) = "c.lineCap = (" ++ showJS a1 ++ ")"
  show (LineJoin (a1)) = "c.lineJoin = (" ++ showJS a1 ++ ")"
  show (LineTo (a1,a2)) = "c.lineTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (LineWidth (a1)) = "c.lineWidth = (" ++ showJS a1 ++ ")"
  show (MiterLimit (a1)) = "c.miterLimit = (" ++ showJS a1 ++ ")"
  show (MoveTo (a1,a2)) = "c.moveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (QuadraticCurveTo (a1,a2,a3,a4)) = "c.quadraticCurveTo(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (Rect (a1,a2,a3,a4)) = "c.rect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show Restore = "c.restore()"
  show (Rotate (a1)) = "c.rotate(" ++ showJS a1 ++ ")"
  show (Scale (a1,a2)) = "c.scale(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show Save = "c.save()"
  show (SetTransform (a1,a2,a3,a4,a5,a6)) = "c.setTransform(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show Stroke = "c.stroke()"
  show (StrokeRect (a1,a2,a3,a4)) = "c.strokeRect(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ ")"
  show (StrokeText (a1,a2,a3)) = "c.strokeText(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ ")"
  show (StrokeStyle (a1)) = "c.strokeStyle = (" ++ showJS a1 ++ ")"
  show (ShadowBlur (a1)) = "c.shadowBlur(" ++ showJS a1 ++ ")"
  show (ShadowColor (a1)) = "c.shadowColor(" ++ showJS a1 ++ ")"
  show (ShadowOffsetX (a1)) = "c.shadowOffsetX(" ++ showJS a1 ++ ")"
  show (ShadowOffsetY (a1)) = "c.shadowOffsetY(" ++ showJS a1 ++ ")"
  show (TextAlign (a1)) = "c.textAlign = (" ++ showJS a1 ++ ")"
  show (TextBaseline (a1)) = "c.textBaseline = (" ++ showJS a1 ++ ")"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "c.transform(" ++ showJS a1 ++ "," ++ showJS a2 ++ "," ++ showJS a3 ++ "," ++ showJS a4 ++ "," ++ showJS a5 ++ "," ++ showJS a6 ++ ")"
  show (Translate (a1,a2)) = "c.translate(" ++ showJS a1 ++ "," ++ showJS a2 ++ ")"
  show (Specials sp) = show sp

-- DSL

arc :: (Float,Float,Float,Float,Float,Bool) -> Canvas ()
arc = Command . Arc

arcTo :: (Float,Float,Float,Float,Float) -> Canvas ()
arcTo = Command . ArcTo

beginPath :: () -> Canvas ()
beginPath () = Command BeginPath

bezierCurveTo :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
bezierCurveTo = Command . BezierCurveTo

drawImage :: (Image,[Float]) -> Canvas ()
drawImage = Command . DrawImage

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Command . ClearRect

clip :: () -> Canvas ()
clip () = Command Clip

closePath :: () -> Canvas ()
closePath () = Command ClosePath

fill :: () -> Canvas ()
fill () = Command Fill

fillRect :: (Float,Float,Float,Float) -> Canvas ()
fillRect = Command . FillRect

fillStyle :: Style a => a -> Canvas ()
fillStyle = Command . FillStyle

fillText :: (String,Float,Float) -> Canvas ()
fillText = Command . FillText

font :: String -> Canvas ()
font = Command . Font

globalAlpha :: Float -> Canvas ()
globalAlpha = Command . GlobalAlpha

globalCompositeOperation :: String -> Canvas ()
globalCompositeOperation = Command . GlobalCompositeOperation

lineCap :: String -> Canvas ()
lineCap = Command . LineCap

lineJoin :: String -> Canvas ()
lineJoin = Command . LineJoin

lineTo :: (Float,Float) -> Canvas ()
lineTo = Command . LineTo

lineWidth :: Float -> Canvas ()
lineWidth = Command . LineWidth

miterLimit :: Float -> Canvas ()
miterLimit = Command . MiterLimit

moveTo :: (Float,Float) -> Canvas ()
moveTo = Command . MoveTo

quadraticCurveTo :: (Float,Float,Float,Float) -> Canvas ()
quadraticCurveTo = Command . QuadraticCurveTo

rect :: (Float,Float,Float,Float) -> Canvas ()
rect = Command . Rect

restore :: () -> Canvas ()
restore () = Command Restore

rotate :: Float -> Canvas ()
rotate = Command . Rotate

scale :: (Float,Float) -> Canvas ()
scale = Command . Scale

save :: () -> Canvas ()
save () = Command Save

setTransform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
setTransform = Command . SetTransform

stroke :: () -> Canvas ()
stroke () = Command Stroke

strokeRect :: (Float,Float,Float,Float) -> Canvas ()
strokeRect = Command . StrokeRect

strokeText :: (String,Float,Float) -> Canvas ()
strokeText = Command . StrokeText

strokeStyle :: String -> Canvas ()
strokeStyle = Command . StrokeStyle

shadowBlur :: Float -> Canvas ()
shadowBlur = Command . ShadowBlur

shadowColor :: String -> Canvas ()
shadowColor = Command . ShadowColor

shadowOffsetX :: Float -> Canvas ()
shadowOffsetX = Command . ShadowOffsetX

shadowOffsetY :: Float -> Canvas ()
shadowOffsetY = Command . ShadowOffsetY

textAlign :: String -> Canvas ()
textAlign = Command . TextAlign

textBaseline :: String -> Canvas ()
textBaseline = Command . TextBaseline

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Command . Transform

translate :: (Float,Float) -> Canvas ()
translate = Command . Translate

