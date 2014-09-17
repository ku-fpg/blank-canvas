{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import Graphics.Blank.Canvas
import Graphics.Blank.JavaScript
import Data.Text (Text)

instance Show Method where
  show (Arc (a1,a2,a3,a4,a5,a6)) = "arc(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsBool a6 ++ ")"
  show (ArcTo (a1,a2,a3,a4,a5)) = "arcTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ ")"
  show BeginPath = "beginPath()"
  show (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "bezierCurveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsFloat a6 ++ ")"
  show (ClearRect (a1,a2,a3,a4)) = "clearRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show Clip = "clip()"
  show ClosePath = "closePath()"
  show (DrawImage (a1,a2)) = "drawImage(" ++ jsImage a1 ++ "," ++ jsList jsFloat a2 ++ ")"
  show Fill = "fill()"
  show (FillRect (a1,a2,a3,a4)) = "fillRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (FillStyle (a1)) = "fillStyle = (" ++ jsStyle a1 ++ ")"
  show (FillText (a1,a2,a3)) = "fillText(" ++ jsText a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ ")"
  show (Font (a1)) = "font = (" ++ jsText a1 ++ ")"
  show (GlobalAlpha (a1)) = "globalAlpha = (" ++ jsFloat a1 ++ ")"
  show (GlobalCompositeOperation (a1)) = "globalCompositeOperation = (" ++ jsText a1 ++ ")"
  show (LineCap (a1)) = "lineCap = (" ++ jsLineEndCap a1 ++ ")"
  show (LineJoin (a1)) = "lineJoin = (" ++ jsLineJoinCorner a1 ++ ")"
  show (LineTo (a1,a2)) = "lineTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show (LineWidth (a1)) = "lineWidth = (" ++ jsFloat a1 ++ ")"
  show (MiterLimit (a1)) = "miterLimit = (" ++ jsFloat a1 ++ ")"
  show (MoveTo (a1,a2)) = "moveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show (PutImageData (a1,a2)) = "putImageData(" ++ jsImageData a1 ++ "," ++ jsList jsFloat a2 ++ ")"
  show (QuadraticCurveTo (a1,a2,a3,a4)) = "quadraticCurveTo(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (Rect (a1,a2,a3,a4)) = "rect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show Restore = "restore()"
  show (Rotate (a1)) = "rotate(" ++ jsFloat a1 ++ ")"
  show Save = "save()"
  show (Scale (a1,a2)) = "scale(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ ")"
  show (SetTransform (a1,a2,a3,a4,a5,a6)) = "setTransform(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ "," ++ jsFloat a5 ++ "," ++ jsFloat a6 ++ ")"
  show (ShadowBlur (a1)) = "shadowBlur = (" ++ jsFloat a1 ++ ")"
  show (ShadowColor (a1)) = "shadowColor = (" ++ jsCanvasColor a1 ++ ")"
  show (ShadowOffsetX (a1)) = "shadowOffsetX = (" ++ jsFloat a1 ++ ")"
  show (ShadowOffsetY (a1)) = "shadowOffsetY = (" ++ jsFloat a1 ++ ")"
  show Stroke = "stroke()"
  show (StrokeRect (a1,a2,a3,a4)) = "strokeRect(" ++ jsFloat a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ "," ++ jsFloat a4 ++ ")"
  show (StrokeStyle (a1)) = "strokeStyle = (" ++ jsStyle a1 ++ ")"
  show (StrokeText (a1,a2,a3)) = "strokeText(" ++ jsText a1 ++ "," ++ jsFloat a2 ++ "," ++ jsFloat a3 ++ ")"
  show (TextAlign (a1)) = "textAlign = (" ++ jsTextAnchorAlignment a1 ++ ")"
  show (TextBaseline (a1)) = "textBaseline = (" ++ jsTextBaselineAlignment a1 ++ ")"
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

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Method . ClearRect

clip :: () -> Canvas ()
clip () = Method Clip

closePath :: () -> Canvas ()
closePath () = Method ClosePath

-- | 'drawImage' takes 2, 4, or 8 'Float' arguments. See 'drawImageAt', 'drawImageSize',
--   and 'drawImageCrop' for variants with exact numbers of arguments.
drawImage :: Image image => (image,[Float]) -> Canvas ()
drawImage = Method . DrawImage

-- | Draws an image onto the canvas at the given x- and y-coordinates.
drawImageAt :: Image image => (image, Float, Float) -> Canvas ()
drawImageAt (img, dx, dy) = Method $ DrawImage (img, [dx, dy])

-- | Acts like 'drawImageAt', but with two extra 'Float' arguments. The third and fourth
--   'Float's specify the width and height of the image, respectively.
drawImageSize :: Image image => (image, Float, Float, Float, Float) -> Canvas ()
drawImageSize (img, dx, dy, dw, dh) = Method $ DrawImage (img, [dx, dy, dw, dh])

-- | Acts like 'drawImageSize', but with four extra 'Float' arguments before the arguments
--   of 'drawImageSize'. The first and second 'Float's specify the x- and y-coordinates at
--   which the image begins to crop. The third and fourth 'Float's specify the width and
--   height of the cropped image.
-- 
-- @
-- 'drawImageCrop' img 0 0 dw dh dx dy dw dh = 'drawImageSize' = dx dy dw dh
-- @
drawImageCrop :: Image image => (image, Float, Float, Float, Float, Float, Float, Float, Float) -> Canvas ()
drawImageCrop (img, sx, sy, sw, sh, dx, dy, dw, dh)
  = Method $ DrawImage (img, [sx, sy, sw, sh, dx, dy, dw, dh])

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

lineCap :: LineEndCap -> Canvas ()
lineCap = Method . LineCap

lineJoin :: LineJoinCorner -> Canvas ()
lineJoin = Method . LineJoin

lineTo :: (Float,Float) -> Canvas ()
lineTo = Method . LineTo

lineWidth :: Float -> Canvas ()
lineWidth = Method . LineWidth

miterLimit :: Float -> Canvas ()
miterLimit = Method . MiterLimit

moveTo :: (Float,Float) -> Canvas ()
moveTo = Method . MoveTo

-- | 'putImageData' takes 2 or 6 'Float' arguments. See `putImageDataAt' and
--   `putImageDataDirty' for variants with exact numbers of arguments.
putImageData :: (ImageData,[Float]) -> Canvas ()
putImageData = Method . PutImageData

-- | Writes 'ImageData' to the canvas at the given x- and y-coordinates.
putImageDataAt :: (ImageData, Float, Float) -> Canvas ()
putImageDataAt (imgData, dx, dy) = Method $ PutImageData (imgData, [dx, dy])

-- | Acts like 'putImageDataDirty', but with four extra 'Float' arguments that specify
--   which region of the 'ImageData' (the dirty rectangle) should be drawn. The third
--   and fourth 'Float's specify the dirty rectangle's x- and y- coordinates, and the
--   fifth and sixth 'Float's specify the dirty rectangle's width and height.
--   
-- @
-- 'putImageDataDirty' imgData dx dy 0 0 w h = 'putImageDataAt' imgData dx dy
--   where (w, h) = case imgData of ImageData w' h' _ -> (w', h')
-- @
putImageDataDirty :: (ImageData, Float, Float, Float, Float, Float, Float) -> Canvas ()
putImageDataDirty (imgData, dx, dy, dirtyX, dirtyY, dirtyWidth, dirtyHeight)
  = Method $ PutImageData (imgData, [dx, dy, dirtyX, dirtyY, dirtyWidth, dirtyHeight])

quadraticCurveTo :: (Float,Float,Float,Float) -> Canvas ()
quadraticCurveTo = Method . QuadraticCurveTo

rect :: (Float,Float,Float,Float) -> Canvas ()
rect = Method . Rect

restore :: () -> Canvas ()
restore () = Method Restore

rotate :: Float -> Canvas ()
rotate = Method . Rotate

save :: () -> Canvas ()
save () = Method Save

scale :: (Float,Float) -> Canvas ()
scale = Method . Scale

setTransform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
setTransform = Method . SetTransform

shadowBlur :: Float -> Canvas ()
shadowBlur = Method . ShadowBlur

shadowColor :: CanvasColor canvasColor => canvasColor -> Canvas ()
shadowColor = Method . ShadowColor

shadowOffsetX :: Float -> Canvas ()
shadowOffsetX = Method . ShadowOffsetX

shadowOffsetY :: Float -> Canvas ()
shadowOffsetY = Method . ShadowOffsetY

stroke :: () -> Canvas ()
stroke () = Method Stroke

strokeRect :: (Float,Float,Float,Float) -> Canvas ()
strokeRect = Method . StrokeRect

strokeStyle :: Style style => style -> Canvas ()
strokeStyle = Method . StrokeStyle

strokeText :: (Text,Float,Float) -> Canvas ()
strokeText = Method . StrokeText

textAlign :: TextAnchorAlignment -> Canvas ()
textAlign = Method . TextAlign

textBaseline :: TextBaselineAlignment -> Canvas ()
textBaseline = Method . TextBaseline

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Method . Transform

translate :: (Float,Float) -> Canvas ()
translate = Method . Translate

