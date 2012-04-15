module Graphics.Blank.Generated where

import Graphics.Blank.Canvas

instance Show Command where
  show BeginPath = "c.beginPath();"
  show (ClearRect (a1,a2,a3,a4)) = "c.clearRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show ClosePath = "c.closePath();"
  show Fill = "c.fill();"
  show (FillStyle (a1)) = "c.fillStyle = (" ++ show a1 ++ ");"
  show (LineCap (a1)) = "c.lineCap(" ++ show a1 ++ ");"
  show (LineTo (a1,a2)) = "c.lineTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show (LineWidth (a1)) = "c.lineWidth = (" ++ showJ a1 ++ ");"
  show (MiterLimit (a1)) = "c.miterLimit = (" ++ showJ a1 ++ ");"
  show (MoveTo (a1,a2)) = "c.moveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Restore = "c.restore();"
  show (Rotate (a1)) = "c.rotate(" ++ showJ a1 ++ ");"
  show (Scale (a1,a2)) = "c.scale(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Save = "c.save();"
  show Stroke = "c.stroke();"
  show (StrokeStyle (a1)) = "c.strokeStyle = (" ++ show a1 ++ ");"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "c.transform(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (Translate (a1,a2)) = "c.translate(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"

-- DSL

beginPath :: Canvas ()
beginPath = Command BeginPath

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Command . ClearRect

closePath :: Canvas ()
closePath = Command ClosePath

fill :: Canvas ()
fill = Command Fill

fillStyle :: String -> Canvas ()
fillStyle = Command . FillStyle

lineCap :: String -> Canvas ()
lineCap = Command . LineCap

lineTo :: (Float,Float) -> Canvas ()
lineTo = Command . LineTo

lineWidth :: Float -> Canvas ()
lineWidth = Command . LineWidth

miterLimit :: Float -> Canvas ()
miterLimit = Command . MiterLimit

moveTo :: (Float,Float) -> Canvas ()
moveTo = Command . MoveTo

restore :: Canvas ()
restore = Command Restore

rotate :: Float -> Canvas ()
rotate = Command . Rotate

scale :: (Float,Float) -> Canvas ()
scale = Command . Scale

save :: Canvas ()
save = Command Save

stroke :: Canvas ()
stroke = Command Stroke

strokeStyle :: String -> Canvas ()
strokeStyle = Command . StrokeStyle

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Command . Transform

translate :: (Float,Float) -> Canvas ()
translate = Command . Translate

