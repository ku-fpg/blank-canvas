{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import           Data.Monoid ((<>))
import           Data.Text (Text)

import           Graphics.Blank.Canvas
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Types.Font

import           TextShow (TextShow(..), FromTextShow(..), showb, singleton)

instance Show Method where
  showsPrec p = showsPrec p . FromTextShow

instance TextShow Method where
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

-- | @'arc'(x, y, r, sAngle, eAngle, cc)@ creates a circular arc, where
-- 
-- * @x@ is the x-coordinate of the center of the circle
-- 
-- * @y@ is the y-coordinate of the center of the circle
-- 
-- * @r@ is the radius of the circle on which the arc is drawn
-- 
-- * @sAngle@ is the starting angle (where @0@ at the 3 o'clock position of the circle)
-- 
-- * @eAngle@ is the ending angle
-- 
-- * @cc@ is the arc direction, where @True@ indicates counterclockwise and
--   @False@ indicates clockwise.
arc :: (Double, Double, Double, Radians, Radians, Bool) -> Canvas ()
arc = Method . Arc

-- | @'arcTo'(x1, y1, x2, y2, r)@ creates an arc between two tangents,
-- specified by two control points and a radius.
-- 
-- * @x1@ is the x-coordinate of the first control point
-- 
-- * @y1@ is the y-coordinate of the first control point
-- 
-- * @x2@ is the x-coordinate of the second control point
-- 
-- * @y2@ is the y-coordinate of the second control point
-- 
-- * @r@ is the arc's radius
arcTo :: (Double, Double, Double, Double, Double) -> Canvas ()
arcTo = Method . ArcTo

-- | Begins drawing a new path. This will empty the current list of subpaths.
--
-- ==== __Example__
-- 
-- @
-- 'beginPath'()
-- 'moveTo'(20, 20)
-- 'lineTo'(200, 20)
-- 'stroke'()
-- @
beginPath :: () -> Canvas ()
beginPath () = Method BeginPath

-- | @'bezierCurveTo'(cp1x, cp1y, cp2x, cp2y x, y)@ adds a cubic Bézier curve to the path
-- (whereas 'quadraticCurveTo' adds a quadratic Bézier curve).
-- 
-- * @cp1x@ is the x-coordinate of the first control point
-- 
-- * @cp1y@ is the y-coordinate of the first control point
-- 
-- * @cp2x@ is the x-coordinate of the second control point
-- 
-- * @cp2y@ is the y-coordinate of the second control point
-- 
-- * @x@ is the x-coordinate of the end point
-- 
-- * @y@ is the y-coordinate of the end point
bezierCurveTo :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
bezierCurveTo = Method . BezierCurveTo

-- | @'clearRect'(x, y, w, h)@ clears all pixels within the rectangle with upper-left
-- corner @(x, y)@, width @w@, and height @h@ (i.e., sets the pixels to transparent black).
--
-- ==== __Example__
-- 
-- @
-- 'fillStyle' \"red\"
-- 'fillRect'(0, 0, 300, 150)
-- 'clearRect'(20, 20, 100, 50)
-- @
clearRect :: (Double, Double, Double, Double) -> Canvas ()
clearRect = Method . ClearRect

-- | Turns the path currently being built into the current clipping path.
-- Anything drawn after 'clip' is called will only be visible if inside the new
-- clipping path. 
--
-- ==== __Example__
-- 
-- @
-- 'rect'(50, 20, 200, 120)
-- 'stroke'()
-- 'clip'()
-- 'fillStyle' \"red\"
-- 'fillRect'(0, 0, 150, 100)
-- @
clip :: () -> Canvas ()
clip () = Method Clip

-- | Creates a path from the current point back to the start, to close it.
--
-- ==== __Example__
-- 
-- @
-- 'beginPath'()
-- 'moveTo'(20, 20)
-- 'lineTo'(200, 20)
-- 'lineTo'(120, 120)
-- 'closePath'()
-- 'stroke'()
-- @
closePath :: () -> Canvas ()
closePath () = Method ClosePath

-- | drawImage' takes 2, 4, or 8 'Double' arguments. See 'drawImageAt', 'drawImageSize', and 'drawImageCrop' for variants with exact numbers of arguments.
drawImage :: Image image => (image,[Double]) -> Canvas ()
drawImage = Method . DrawImage

-- | Fills the current path with the current 'fillStyle'.
--
-- ==== __Example__
-- 
-- @
-- 'rect'(10, 10, 100, 100)
-- 'fill'()
-- @
fill :: () -> Canvas ()
fill () = Method Fill

-- | @'fillRect'(x, y, w, h)@ draws a filled rectangle with upper-left
-- corner @(x, y)@, width @w@, and height @h@ using the current 'fillStyle'.
--
-- ==== __Example__
-- 
-- @
-- 'fillStyle' \"red\"
-- 'fillRect'(0, 0, 300, 150)
-- @
fillRect :: (Double, Double, Double, Double) -> Canvas ()
fillRect = Method . FillRect

-- | Sets the color, gradient, or pattern used to fill a drawing ('black' by default).
--
-- ==== __Examples__
-- 
-- @
-- 'fillStyle' 'red'
-- 
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- 'fillStyle' grd
-- 
-- img <- 'newImage' \"/myImage.jpg\"
-- pat <- 'createPattern'(img, 'Repeat')
-- 'fillStyle' pat
-- @
fillStyle :: Style style => style -> Canvas ()
fillStyle = Method . FillStyle

-- | @'fillText'(t, x, y)@ fills the text @t@ at position @(x, y)@
-- using the current 'fillStyle'.
--
-- ==== __Example__
-- 
-- @
-- 'font' \"48px serif\"
-- 'fillText'(\"Hello, World!\", 50, 100)
-- @
fillText :: (Text, Double, Double) -> Canvas ()
fillText = Method . FillText

-- | Sets the text context's font properties.
--
-- ==== __Examples__
-- 
-- @
-- 'font' ('defFont' "Gill Sans Extrabold") { 'fontSize' = 40 # 'pt' }
-- 'font' ('defFont' 'sansSerif') { 'fontSize' = 80 # 'percent' }
-- 'font' ('defFont' 'serif') {
--     'fontWeight' = 'bold'
--   , 'fontStyle'  = 'italic'
--   , 'fontSize'   = 'large'
-- }
-- @
font :: CanvasFont canvasFont => canvasFont -> Canvas ()
font = Method . Font

-- | Set the alpha value that is applied to shapes before they are drawn onto the canvas.
globalAlpha :: Alpha -> Canvas ()
globalAlpha = Method . GlobalAlpha

-- | Sets how new shapes should be drawn over existing shapes.
--
-- ==== __Examples__
-- 
-- @
-- 'globalCompositeOperation' \"source-over\"
-- 'globalCompositeOperation' \"destination-atop\"
-- @
globalCompositeOperation :: Text -> Canvas ()
globalCompositeOperation = Method . GlobalCompositeOperation

-- | Sets the 'LineEndCap' to use when drawing the endpoints of lines.
lineCap :: LineEndCap -> Canvas ()
lineCap = Method . LineCap

-- | Sets the 'LineJoinCorner' to use when drawing two connected lines.
lineJoin :: LineJoinCorner -> Canvas ()
lineJoin = Method . LineJoin

-- | @'lineTo'(x, y)@ connects the last point in the subpath to the given @(x, y)@
-- coordinates (without actually drawing it).
--
-- ==== __Example__
-- 
-- @
-- 'beginPath'()
-- 'moveTo'(50, 50)
-- 'lineTo'(200, 50)
-- 'stroke'()
-- @
lineTo :: (Double, Double) -> Canvas ()
lineTo = Method . LineTo

-- | Sets the thickness of lines in pixels (@1.0@ by default).
lineWidth :: Double -> Canvas ()
lineWidth = Method . LineWidth

-- | Sets the maximum miter length (@10.0@ by default) to use when the
-- 'lineWidth' is 'miter'.
miterLimit :: Double -> Canvas ()
miterLimit = Method . MiterLimit

-- | @'moveTo'(x, y)@ moves the starting point of a new subpath to the given @(x, y)@ coordinates.
--
-- ==== __Example__
-- 
-- @
-- 'beginPath'()
-- 'moveTo'(50, 50)
-- 'lineTo'(200, 50)
-- 'stroke'()
-- @
moveTo :: (Double, Double) -> Canvas ()
moveTo = Method . MoveTo

-- | 'putImageData' takes 2 or 6 'Double' arguments. See `putImageDataAt' and `putImageDataDirty' for variants with exact numbers of arguments.
putImageData :: (ImageData, [Double]) -> Canvas ()
putImageData = Method . PutImageData

-- | @'quadraticCurveTo'(cpx, cpy, x, y)@ adds a quadratic Bézier curve to the path
-- (whereas 'bezierCurveTo' adds a cubic Bézier curve).
-- 
-- * @cpx@ is the x-coordinate of the control point
-- 
-- * @cpy@ is the y-coordinate of the control point
-- 
-- * @x@ is the x-coordinate of the end point
-- 
-- * @y@ is the y-coordinate of the end point
quadraticCurveTo :: (Double, Double, Double, Double) -> Canvas ()
quadraticCurveTo = Method . QuadraticCurveTo

-- | @'rect'(x, y, w, h)@ creates a rectangle with an upper-left corner at position
-- @(x, y)@, width @w@, and height @h@ (where width and height are in pixels).
--
-- ==== __Example__
-- 
-- @
-- 'rect'(10, 10, 100, 100)
-- 'fill'()
-- @
rect :: (Double, Double, Double, Double) -> Canvas ()
rect = Method . Rect

-- | Restores the most recently saved canvas by popping the top entry off of the
-- drawing state stack. If there is no state, do nothing.
restore :: () -> Canvas ()
restore () = Method Restore

-- | Applies a rotation transformation to the canvas. When you call functions
-- such as 'fillRect' after 'rotate', the drawings will be rotated clockwise by
-- the angle given to 'rotate' (in radians).
--
-- ==== __Example__
-- 
-- @
-- 'rotate' ('pi'/2)        -- Rotate the canvas 90°
-- 'fillRect'(0, 0, 20, 10) -- Draw a 10x20 rectangle
-- @
rotate :: Radians -> Canvas ()
rotate = Method . Rotate

-- | Saves the entire canvas by pushing the current state onto a stack.
save :: () -> Canvas ()
save () = Method Save

-- | Applies a scaling transformation to the canvas units, where the first argument
-- is the percent to scale horizontally, and the second argument is the percent to
-- scale vertically. By default, one canvas unit is one pixel.
--
-- ==== __Examples__
-- 
-- @
-- 'scale'(0.5, 0.5)        -- Halve the canvas units
-- 'fillRect'(0, 0, 20, 20) -- Draw a 10x10 square
-- 'scale'(-1, 1)           -- Flip the context horizontally
-- @
scale :: (Interval, Interval) -> Canvas ()
scale = Method . Scale

-- | Resets the canvas's transformation matrix to the identity matrix,
-- then calls 'transform' with the given arguments.
setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
setTransform = Method . SetTransform

-- | Sets the blur level for shadows (@0.0@ by default).
shadowBlur :: Double -> Canvas ()
shadowBlur = Method . ShadowBlur

-- | Sets the color used for shadows.
--
-- ==== __Examples__
-- 
-- @
-- 'shadowColor' 'red'
-- 'shadowColor' $ 'rgb' 0 255 0
-- @
shadowColor :: CanvasColor canvasColor => canvasColor -> Canvas ()
shadowColor = Method . ShadowColor

-- | Sets the horizontal distance that a shadow will be offset (@0.0@ by default).
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX = Method . ShadowOffsetX

-- | Sets the vertical distance that a shadow will be offset (@0.0@ by default).
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY = Method . ShadowOffsetY

-- | Draws the current path's strokes with the current 'strokeStyle' ('black' by default).
--
-- ==== __Example__
-- 
-- @
-- 'rect'(10, 10, 100, 100)
-- 'stroke'()
-- @
stroke :: () -> Canvas ()
stroke () = Method Stroke

-- | @'strokeRect'(x, y, w, h)@ draws a rectangle (no fill) with upper-left
-- corner @(x, y)@, width @w@, and height @h@ using the current 'strokeStyle'.
--
-- ==== __Example__
-- 
-- @
-- 'strokeStyle' \"red\"
-- 'strokeRect'(0, 0, 300, 150)
-- @
strokeRect :: (Double, Double, Double, Double) -> Canvas ()
strokeRect = Method . StrokeRect

-- | Sets the color, gradient, or pattern used for strokes.
--
-- ==== __Examples__
-- 
-- @
-- 'strokeStyle' 'red'
-- 
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- 'strokeStyle' grd
-- 
-- img <- 'newImage' \"/myImage.jpg\"
-- pat <- 'createPattern'(img, 'Repeat')
-- 'strokeStyle' pat
-- @
strokeStyle :: Style style => style -> Canvas ()
strokeStyle = Method . StrokeStyle

-- | @'strokeText'(t, x, y)@ draws text @t@ (with no fill) at position @(x, y)@
-- using the current 'strokeStyle'.
--
-- ==== __Example__
-- 
-- @
-- 'font' \"48px serif\"
-- 'strokeText'(\"Hello, World!\", 50, 100)
-- @
strokeText :: (Text,Double, Double) -> Canvas ()
strokeText = Method . StrokeText

-- | Sets the 'TextAnchorAlignment' to use when drawing text.
textAlign :: TextAnchorAlignment -> Canvas ()
textAlign = Method . TextAlign

-- | Sets the 'TextBaselineAlignment' to use when drawing text.
textBaseline :: TextBaselineAlignment -> Canvas ()
textBaseline = Method . TextBaseline

-- | Applies a transformation by multiplying a matrix to the canvas's
-- current transformation. If @'transform'(a, b, c, d, e, f)@ is called, the matrix
-- 
-- @
-- ( a c e )
-- ( b d f )
-- ( 0 0 1 )
-- @
-- 
-- is multiplied by the current transformation. The parameters are:
-- 
-- * @a@ is the horizontal scaling
-- 
-- * @b@ is the horizontal skewing
-- 
-- * @c@ is the vertical skewing
-- 
-- * @d@ is the vertical scaling
-- 
-- * @e@ is the horizontal movement
-- 
-- * @f@ is the vertical movement
transform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
transform = Method . Transform

-- | Applies a translation transformation by remapping the origin (i.e., the (0,0)
-- position) on the canvas. When you call functions such as 'fillRect' after
-- 'translate', the values passed to 'translate' are added to the x- and
-- y-coordinate values. 
--
-- ==== __Example__
-- 
-- @
-- 'translate'(20, 20)
-- 'fillRect'(0, 0, 40, 40) -- Draw a 40x40 square, starting in position (20, 20)
-- @
translate :: (Double, Double) -> Canvas ()
translate = Method . Translate
