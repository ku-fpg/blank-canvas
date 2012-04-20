{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Graphics.Blank.Canvas

instance Show Command where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "c.arc(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showB a6 ++ ");"
  show BeginPath = "c.beginPath();"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "c.bezierCurveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (ClearRect (a1,a2,a3,a4)) = "c.clearRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show ClosePath = "c.closePath();"
  show Fill = "c.fill();"
  show (FillStyle (a1)) = "c.fillStyle = (" ++ show a1 ++ ");"
  show (FillText (a1,a2,a3)) = "c.fillText(" ++ show a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ ");"
  show (Font (a1)) = "c.font = (" ++ show a1 ++ ");"
  show (LineCap (a1)) = "c.lineCap = (" ++ show a1 ++ ");"
  show (LineJoin (a1)) = "c.lineJoin = (" ++ show a1 ++ ");"
  show (LineTo (a1,a2)) = "c.lineTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show (LineWidth (a1)) = "c.lineWidth = (" ++ showJ a1 ++ ");"
  show (MiterLimit (a1)) = "c.miterLimit = (" ++ showJ a1 ++ ");"
  show (MoveTo (a1,a2)) = "c.moveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Restore = "c.restore();"
  show (Rotate (a1)) = "c.rotate(" ++ showJ a1 ++ ");"
  show (Scale (a1,a2)) = "c.scale(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Save = "c.save();"
  show Stroke = "c.stroke();"
  show (StrokeText (a1,a2,a3)) = "c.strokeText(" ++ show a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ ");"
  show (StrokeStyle (a1)) = "c.strokeStyle = (" ++ show a1 ++ ");"
  show (TextAlign (a1)) = "c.textAlign = (" ++ show a1 ++ ");"
  show (TextBaseline (a1)) = "c.textBaseline = (" ++ show a1 ++ ");"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "c.transform(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (Translate (a1,a2)) = "c.translate(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"

-- DSL

arc :: (Float,Float,Float,Float,Float,Bool) -> Canvas ()
arc = Command . Arc

beginPath :: () -> Canvas ()
beginPath () = Command BeginPath

bezierCurveTo :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
bezierCurveTo = Command . BezierCurveTo

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Command . ClearRect

closePath :: () -> Canvas ()
closePath () = Command ClosePath

fill :: () -> Canvas ()
fill () = Command Fill

fillStyle :: String -> Canvas ()
fillStyle = Command . FillStyle

fillText :: (String,Float,Float) -> Canvas ()
fillText = Command . FillText

font :: String -> Canvas ()
font = Command . Font

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

restore :: () -> Canvas ()
restore () = Command Restore

rotate :: Float -> Canvas ()
rotate = Command . Rotate

scale :: (Float,Float) -> Canvas ()
scale = Command . Scale

save :: () -> Canvas ()
save () = Command Save

stroke :: () -> Canvas ()
stroke () = Command Stroke

strokeText :: (String,Float,Float) -> Canvas ()
strokeText = Command . StrokeText

strokeStyle :: String -> Canvas ()
strokeStyle = Command . StrokeStyle

textAlign :: String -> Canvas ()
textAlign = Command . TextAlign

textBaseline :: String -> Canvas ()
textBaseline = Command . TextBaseline

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Command . Transform

translate :: (Float,Float) -> Canvas ()
translate = Command . Translate

