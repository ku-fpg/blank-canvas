{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Data.Text (Text)

import Graphics.Blank.Canvas
import Graphics.Blank.JavaScript

instance Show Method where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "arc(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsBool a6 ++ ")"
  show (ArcTo (a1,a2,a3,a4,a5)) = "arcTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ ")"
  show BeginPath = "beginPath()"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "bezierCurveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsFloat a6 ++ ")"
  show (DrawImage (a1,a2)) = "drawImage(" ++ jsImage a1 ++ "," ++ jsList jsFloat a2 ++ ")"
  show (ClearRect (a1,a2,a3,a4)) = "clearRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show Clip = "clip()"
  show ClosePath = "closePath()"
  show Fill = "fill()"
  show (FillRect (a1,a2,a3,a4)) = "fillRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (FillStyle (a1)) = "fillStyle = (" ++ jsStyle a1 ++ ")"
  show (FillText (a1,a2,a3)) = "fillText(" ++ jsText a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ ")"
  show (Font (a1)) = "font = (" ++ jsText a1 ++ ")"
  show (GlobalAlpha (a1)) = "globalAlpha = (" ++ jsFloat a1 ++ ")"
  show (GlobalCompositeOperation (a1)) = "globalCompositeOperation = (" ++ jsText a1 ++ ")"
  show (LineCap (a1)) = "lineCap = (" ++ jsText a1 ++ ")"
  show (LineJoin (a1)) = "lineJoin = (" ++ jsText a1 ++ ")"
  show (LineTo (a1,a2)) = "lineTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show (LineWidth (a1)) = "lineWidth = (" ++ jsFloat a1 ++ ")"
  show (MiterLimit (a1)) = "miterLimit = (" ++ jsFloat a1 ++ ")"
  show (MoveTo (a1,a2)) = "moveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show (PutImageData (a1,a2)) = "putImageData(" ++ jsImageData a1 ++ "," ++ jsList jsFloat a2 ++ ")"
  show (QuadraticCurveTo (a1,a2,a3,a4)) = "quadraticCurveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (Rect (a1,a2,a3,a4)) = "rect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show Restore = "restore()"
  show (Rotate (a1)) = "rotate(" ++ jsFloat a1 ++ ")"
  show (Scale (a1,a2)) = "scale(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show Save = "save()"
  show (SetTransform (a1,a2,a3,a4,a5,a6)) = "setTransform(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsFloat a6 ++ ")"
  show Stroke = "stroke()"
  show (StrokeRect (a1,a2,a3,a4)) = "strokeRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (StrokeText (a1,a2,a3)) = "strokeText(" ++ jsText a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ ")"
  show (StrokeStyle (a1)) = "strokeStyle = (" ++ jsStyle a1 ++ ")"
  show (ShadowBlur (a1)) = "shadowBlur = (" ++ jsFloat a1 ++ ")"
  show (ShadowColor (a1)) = "shadowColor = (" ++ jsStyle a1 ++ ")"
  show (ShadowOffsetX (a1)) = "shadowOffsetX = (" ++ jsFloat a1 ++ ")"
  show (ShadowOffsetY (a1)) = "shadowOffsetY = (" ++ jsFloat a1 ++ ")"
  show (TextAlign (a1)) = "textAlign = (" ++ jsText a1 ++ ")"
  show (TextBaseline (a1)) = "textBaseline = (" ++ jsText a1 ++ ")"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "transform(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsFloat a6 ++ ")"
  show (Translate (a1,a2)) = "translate(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"

-- DSL

arc :: (Float,Float,Float,Float,Float,Bool) -> Canvas ()
arc = Method . Arc

arcTo :: (Float,Float,Float,Float,Float) -> Canvas ()
arcTo = Method . ArcTo

beginPath :: () -> Canvas ()
beginPath () = Method BeginPath

bezierCurveTo :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
bezierCurveTo = Method . BezierCurveTo

-- | 'drawImage' takes 2, 4 or 8 floats arguments
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

fillText :: (Text,Float,Float) -> Canvas ()
fillText = Method . FillText

font :: Text -> Canvas ()
font = Method . Font

globalAlpha :: Float -> Canvas ()
globalAlpha = Method . GlobalAlpha

globalCompositeOperation :: Text -> Canvas ()
globalCompositeOperation = Method . GlobalCompositeOperation

lineCap :: Text -> Canvas ()
lineCap = Method . LineCap

lineJoin :: Text -> Canvas ()
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

strokeText :: (Text,Float,Float) -> Canvas ()
strokeText = Method . StrokeText

strokeStyle :: Style style => style -> Canvas ()
strokeStyle = Method . StrokeStyle

shadowBlur :: Float -> Canvas ()
shadowBlur = Method . ShadowBlur

shadowColor :: CanvasColor color => color -> Canvas ()
shadowColor = Method . ShadowColor

shadowOffsetX :: Float -> Canvas ()
shadowOffsetX = Method . ShadowOffsetX

shadowOffsetY :: Float -> Canvas ()
shadowOffsetY = Method . ShadowOffsetY

textAlign :: Text -> Canvas ()
textAlign = Method . TextAlign

textBaseline :: Text -> Canvas ()
textBaseline = Method . TextBaseline

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Method . Transform

translate :: (Float,Float) -> Canvas ()
translate = Method . Translate

