{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import           Data.Monoid ((<>))
import           Data.Text (Text)

import           Graphics.Blank.Canvas
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types.Font

import           Prelude hiding (Show)

import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text (showb, showbPrec, singleton, toString)

instance S.Show Method where
  showsPrec p = (++) . toString . showbPrec p

instance T.Show Method where
  showb (Arc (a1,a2,a3,a4,a5,a6)) = "arc("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ','
         <> jsDouble a5 <> singleton ',' <> jsBool a6   <> singleton ')'
  showb (ArcTo (a1,a2,a3,a4,a5)) = "arcTo("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ',' <> jsDouble a3 <> singleton ','
         <> jsDouble a4 <> singleton ',' <> jsDouble a5 <> singleton ')'
  showb BeginPath = "beginPath()"
  showb (BezierCurveTo (a1,a2,a3,a4,a5,a6)) = "bezierCurveTo("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ','
         <> jsDouble a5 <> singleton ',' <> jsDouble a6 <> singleton ')'
  showb (ClearRect (a1,a2,a3,a4)) = "clearRect("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ')'
  showb Clip = "clip()"
  showb ClosePath = "closePath()"
  showb (DrawImage (a1,a2)) = "drawImage(" <> jsImage a1 <> singleton ',' <> jsList jsDouble a2 <> singleton ')'
  showb Fill = "fill()"
  showb (FillRect (a1,a2,a3,a4)) = "fillRect("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ')'
  showb (FillStyle (a1)) = "fillStyle = (" <> jsStyle a1 <> singleton ')'
  showb (FillText (a1,a2,a3)) = "fillText(" <> jsText a1 <> singleton ',' <> jsDouble a2 <> singleton ',' <> jsDouble a3 <> singleton ')'
  showb (Font (a1)) = "font = (" <> jsCanvasFont a1 <> singleton ')'
  showb (GlobalAlpha (a1)) = "globalAlpha = (" <> jsDouble a1 <> singleton ')'
  showb (GlobalCompositeOperation (a1)) = "globalCompositeOperation = (" <> jsText a1 <> singleton ')'
  showb (LineCap (a1)) = "lineCap = (" <> jsLineEndCap a1 <> singleton ')'
  showb (LineJoin (a1)) = "lineJoin = (" <> jsLineJoinCorner a1 <> singleton ')'
  showb (LineTo (a1,a2)) = "lineTo(" <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ')'
  showb (LineWidth (a1)) = "lineWidth = (" <> jsDouble a1 <> singleton ')'
  showb (MiterLimit (a1)) = "miterLimit = (" <> jsDouble a1 <> singleton ')'
  showb (MoveTo (a1,a2)) = "moveTo(" <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ')'
  showb (PutImageData (a1,a2)) = "putImageData(" <> jsImageData a1 <> singleton ',' <> jsList jsDouble a2 <> singleton ')'
  showb (QuadraticCurveTo (a1,a2,a3,a4)) = "quadraticCurveTo("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ')'
  showb (Rect (a1,a2,a3,a4)) = "rect("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ')'
  showb Restore = "restore()"
  showb (Rotate (a1)) = "rotate(" <> jsDouble a1 <> singleton ')'
  showb Save = "save()"
  showb (Scale (a1,a2)) = "scale(" <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ')'
  showb (SetTransform (a1,a2,a3,a4,a5,a6)) = "setTransform("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ','
         <> jsDouble a5 <> singleton ',' <> jsDouble a6 <> singleton ')'
  showb (ShadowBlur (a1)) = "shadowBlur = (" <> jsDouble a1 <> singleton ')'
  showb (ShadowColor (a1)) = "shadowColor = (" <> jsCanvasColor a1 <> singleton ')'
  showb (ShadowOffsetX (a1)) = "shadowOffsetX = (" <> jsDouble a1 <> singleton ')'
  showb (ShadowOffsetY (a1)) = "shadowOffsetY = (" <> jsDouble a1 <> singleton ')'
  showb Stroke = "stroke()"
  showb (StrokeRect (a1,a2,a3,a4)) = "strokeRect("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ')'
  showb (StrokeStyle (a1)) = "strokeStyle = (" <> jsStyle a1 <> singleton ')'
  showb (StrokeText (a1,a2,a3)) = "strokeText(" <> jsText a1 <> singleton ',' <> jsDouble a2 <> singleton ',' <> jsDouble a3 <> singleton ')'
  showb (TextAlign (a1)) = "textAlign = (" <> jsTextAnchorAlignment a1 <> singleton ')'
  showb (TextBaseline (a1)) = "textBaseline = (" <> jsTextBaselineAlignment a1 <> singleton ')'
  showb (Transform (a1,a2,a3,a4,a5,a6)) = "transform("
         <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ','
         <> jsDouble a3 <> singleton ',' <> jsDouble a4 <> singleton ','
         <> jsDouble a5 <> singleton ',' <> jsDouble a6 <> singleton ')'
  showb (Translate (a1,a2)) = "translate(" <> jsDouble a1 <> singleton ',' <> jsDouble a2 <> singleton ')'

-- DSL

arc :: (Double, Double, Double, Double, Double, Bool) -> Canvas ()
arc = Method . Arc

arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo = Method . ArcTo

beginPath :: () -> Canvas ()
beginPath () = Method BeginPath

bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo = Method . BezierCurveTo

clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect = Method . ClearRect

clip :: () -> Canvas ()
clip () = Method Clip

closePath :: () -> Canvas ()
closePath () = Method ClosePath

-- | drawImage' takes 2, 4, or 8 'Double' arguments. See 'drawImageAt', 'drawImageSize', and 'drawImageCrop' for variants with exact numbers of arguments.
drawImage :: Image image => (image,[Double]) -> Canvas ()
drawImage = Method . DrawImage

fill :: () -> Canvas ()
fill () = Method Fill

fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect = Method . FillRect

fillStyle :: Style style => style -> Canvas ()
fillStyle = Method . FillStyle

fillText :: (Text, Double, Double) -> Canvas ()
fillText = Method . FillText

font :: CanvasFont canvasFont => canvasFont -> Canvas ()
font = Method . Font

globalAlpha :: Double -> Canvas ()
globalAlpha = Method . GlobalAlpha

globalCompositeOperation :: Text -> Canvas ()
globalCompositeOperation = Method . GlobalCompositeOperation

lineCap :: LineEndCap -> Canvas ()
lineCap = Method . LineCap

lineJoin :: LineJoinCorner -> Canvas ()
lineJoin = Method . LineJoin

lineTo :: (Double, Double) -> Canvas ()
lineTo = Method . LineTo

lineWidth :: Double -> Canvas ()
lineWidth = Method . LineWidth

miterLimit :: Double -> Canvas ()
miterLimit = Method . MiterLimit

moveTo :: (Double, Double) -> Canvas ()
moveTo = Method . MoveTo

-- | 'putImageData' takes 2 or 6 'Double' arguments. See `putImageDataAt' and `putImageDataDirty' for variants with exact numbers of arguments.
putImageData :: (ImageData, [Double]) -> Canvas ()
putImageData = Method . PutImageData

quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo = Method . QuadraticCurveTo

rect :: (Double, Double, Double, Double) -> Canvas ()
rect = Method . Rect

restore :: () -> Canvas ()
restore () = Method Restore

rotate :: Double -> Canvas ()
rotate = Method . Rotate

save :: () -> Canvas ()
save () = Method Save

scale :: (Double, Double) -> Canvas ()
scale = Method . Scale

setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
setTransform = Method . SetTransform

shadowBlur :: Double -> Canvas ()
shadowBlur = Method . ShadowBlur

shadowColor :: CanvasColor canvasColor => canvasColor -> Canvas ()
shadowColor = Method . ShadowColor

shadowOffsetX :: Double -> Canvas ()
shadowOffsetX = Method . ShadowOffsetX

shadowOffsetY :: Double -> Canvas ()
shadowOffsetY = Method . ShadowOffsetY

stroke :: () -> Canvas ()
stroke () = Method Stroke

strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect = Method . StrokeRect

strokeStyle :: Style style => style -> Canvas ()
strokeStyle = Method . StrokeStyle

strokeText :: (Text,Double, Double) -> Canvas ()
strokeText = Method . StrokeText

textAlign :: TextAnchorAlignment -> Canvas ()
textAlign = Method . TextAlign

textBaseline :: TextBaselineAlignment -> Canvas ()
textBaseline = Method . TextBaseline

transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform = Method . Transform

translate :: (Double, Double) -> Canvas ()
translate = Method . Translate

