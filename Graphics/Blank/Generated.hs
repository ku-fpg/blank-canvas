{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.Generated where

import qualified Data.Text                 as ST
import           Data.Text.Lazy            (fromStrict)

import           Graphics.Blank.Canvas
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Types.Font

import           Graphics.Blank.Instr

import           Prelude.Compat

import qualified Network.JavaScript as JS

-- import           TextShow (TextShow(..), FromTextShow(..), singleton)
import           Graphics.Blank.Types.Cursor(CanvasCursor, jsbCanvasCursor)

{-
instance InstrShow a => InstrShow (Prim a) where
    showiPrec _ = showi
    showi (PseudoProcedure f _) = showi f
--    showi (Command c _)     = showi c
--    showi (MethodAudio a _) = showi a
    showi (Query q _) = showi q
-}

{-
instance InstrShow MethodAudio where
  showiPrec _ = showi
  showi (PlayAudio audio)                    = jsAudio audio <> ".play()"
  showi (PauseAudio audio)                   = jsAudio audio <> ".pause()"
  showi (SetCurrentTimeAudio  (audio, time)) = jsAudio audio <> ".currentTime = " <> jsDouble time <> singleton ';'
  showi (SetLoopAudio         (audio, loop)) = jsAudio audio <> ".loop = " <> jsBool loop <> singleton ';'
  showi (SetMutedAudio        (audio, mute)) = jsAudio audio <> ".muted = " <> jsBool mute <> singleton ';'
  showi (SetPlaybackRateAudio (audio, rate)) = jsAudio audio <> ".playbackRate = " <> jsDouble rate <> singleton ';'
  showi (SetVolumeAudio       (audio, vol))  = jsAudio audio <> ".volume = " <> jsDouble vol <> singleton ';'
-}
  -- showi (CurrentTimeAudio audio) = jsAudio audio <> ".currentTime"

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
arc (a1,a2,a3,a4,a5,a6) = primitiveMethod "arc"
  [showJSB a1, showJSB a2, showJSB a3, showJSB a4, showJSB a5, showJSB a6]

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
arcTo (a1,a2,a3,a4,a5) = primitiveMethod "arcTo"
  [ showJSB a1, showJSB a2, showJSB a3, showJSB a4, showJSB a5]

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
beginPath () = primitiveMethod "beginPath" []

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
bezierCurveTo (a1,a2,a3,a4,a5,a6) = primitiveMethod "bezierCurveTo"
  [ showJSB a1, showJSB a2, showJSB a3, showJSB a4, showJSB a5, showJSB a6]

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
clearRect (a1,a2,a3,a4) = primitiveMethod "clearRect" 
  [ showJSB a1, showJSB a2, showJSB a3, showJSB a4 ]

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
clip () = primitiveMethod "clip" []

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
closePath () = primitiveMethod "closePath" []

-- | drawImage' takes 2, 4, or 8 'Double' arguments. See 'drawImageAt', 'drawImageSize', and 'drawImageCrop' for variants with exact numbers of arguments.
drawImage :: Image image => (image,[Double]) -> Canvas ()
drawImage (img, args) = primitiveMethod "drawImage" 
  (jsbImage img : map showJSB args)

-- | Fills the current path with the current 'fillStyle'.
--
-- ==== __Example__
--
-- @
-- 'rect'(10, 10, 100, 100)
-- 'fill'()
-- @
fill :: () -> Canvas ()
fill () = primitiveMethod "fill" []

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
fillRect (a1, a2, a3, a4) = primitiveMethod "fillRect" 
  [ showJSB a1, showJSB a2, showJSB a3, showJSB a4 ]

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
fillStyle st = primitiveAttribute "fillStyle" [jsbStyle st]

-- | @'fillText'(t, x, y)@ fills the text @t@ at position @(x, y)@
-- using the current 'fillStyle'.
--
-- ==== __Example__
--
-- @
-- 'font' \"48px serif\"
-- 'fillText'(\"Hello, World!\", 50, 100)
-- @
fillText :: (ST.Text, Double, Double) -> Canvas ()
fillText (t, a, b) = primitiveMethod "fillText"
  [showJSB t, showJSB a, showJSB b]

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
font f = primitiveAttribute "font" [jsbCanvasFont f]

-- | Set the alpha value that is applied to shapes before they are drawn onto the canvas.
globalAlpha :: Alpha -> Canvas ()
globalAlpha a = primitiveAttribute "globalAlpha" [showJSB a]

-- | Sets how new shapes should be drawn over existing shapes.
--
-- ==== __Examples__
--
-- @
-- 'globalCompositeOperation' \"source-over\"
-- 'globalCompositeOperation' \"destination-atop\"
-- @
globalCompositeOperation :: ST.Text -> Canvas ()
globalCompositeOperation t = 
  primitiveAttribute "globalCompositeOperation" [showJSB t]

-- | Sets the 'LineEndCap' to use when drawing the endpoints of lines.
lineCap :: LineEndCap -> Canvas ()
lineCap lec = primitiveAttribute "lineCap" [showJSB lec]

-- | Sets the 'LineJoinCorner' to use when drawing two connected lines.
lineJoin :: LineJoinCorner -> Canvas ()
lineJoin ljc = primitiveAttribute "lineJoin" [showJSB ljc]

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
lineTo (a1,a2) = primitiveMethod "lineTo" [showJSB a1, showJSB a2]

-- | Sets the thickness of lines in pixels (@1.0@ by default).
lineWidth :: Double -> Canvas ()
lineWidth a = primitiveAttribute "lineWidth" [showJSB a]

-- | Sets the maximum miter length (@10.0@ by default) to use when the
-- 'lineWidth' is 'miter'.
miterLimit :: Double -> Canvas ()
miterLimit a = primitiveAttribute "miterLimit" [showJSB a]

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
moveTo (a1,a2) = primitiveMethod "moveTo" [showJSB a1,showJSB  a2]
{-
playAudio :: Audio audio => audio -> Canvas ()
playAudio = primitive . MethodAudio . PlayAudio

pauseAudio :: Audio audio => audio -> Canvas ()
pauseAudio = primitive . MethodAudio . PauseAudio

-- | Sets the current position of the audio value (in seconds).
setCurrentTimeAudio :: Audio audio => (audio, Double) -> Canvas ()
setCurrentTimeAudio = primitive . MethodAudio . SetCurrentTimeAudio

-- | Set whether the audio value is set to loop (True) or not (False)
setLoopAudio :: Audio audio => (audio, Bool) -> Canvas ()
setLoopAudio = primitive . MethodAudio . SetLoopAudio

-- | Set the audio value to muted (True) or unmuted (False)
setMutedAudio :: Audio audio => (audio, Bool) -> Canvas ()
setMutedAudio = primitive . MethodAudio . SetMutedAudio

-- | Set the playback value, as a multiplier (2.0 is twice as fast, 0.5 is half speec, -2.0 is backwards, twice as fast)
setPlaybackRateAudio :: Audio audio => (audio, Double) -> Canvas ()
setPlaybackRateAudio = primitive . MethodAudio . SetPlaybackRateAudio

-- | Adjusts the volume.  Scaled from 0.0 - 1.0, any number outside of this range will result in an error
setVolumeAudio :: Audio audio => (audio, Double) -> Canvas ()
setVolumeAudio = primitive . MethodAudio . SetVolumeAudio
-}
-- | 'putImageData' takes 2 or 6 'Double' arguments. See `putImageDataAt' and `putImageDataDirty' for variants with exact numbers of arguments.
putImageData :: (ImageData, [Double]) -> Canvas ()
putImageData (a1,a2) =
  primitiveMethod "putImageData" (showJSB a1 : map showJSB a2)
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
quadraticCurveTo (a1,a2,a3,a4) = primitiveMethod "quadraticCurveTo"
  [showJSB a1, showJSB a2, showJSB a3, showJSB a4]

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
rect (a1,a2,a3,a4) = primitiveMethod "rect"
  [showJSB a1, showJSB a2, showJSB a3, showJSB a4]

-- | Restores the most recently saved canvas by popping the top entry off of the
-- drawing state stack. If there is no state, do nothing.
restore :: () -> Canvas ()
restore () = primitiveMethod "restore" []

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
rotate r = primitiveMethod "rotate" [showJSB r]

-- | Saves the entire canvas by pushing the current state onto a stack.
save :: () -> Canvas ()
save () = primitiveMethod "save" []

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
scale (a1,a2) = primitiveMethod "scale" [showJSB a1, showJSB a2]

-- | Resets the canvas's transformation matrix to the identity matrix,
-- then calls 'transform' with the given arguments.
setTransform :: (Double, Double, Double, Double, Double, Double) -> Canvas ()
setTransform (a1,a2,a3,a4,a5,a6) = primitiveMethod "setTransform"
  [showJSB a1, showJSB a2, showJSB a2, showJSB a4, showJSB a5, showJSB a6]

-- | Sets the blur level for shadows (@0.0@ by default).
shadowBlur :: Double -> Canvas ()
shadowBlur a1 = primitiveAttribute "shadowBlur" [showJSB a1]

-- | Sets the color used for shadows.
--
-- ==== __Examples__
--
-- @
-- 'shadowColor' 'red'
-- 'shadowColor' $ 'rgb' 0 255 0
-- @
shadowColor :: CanvasColor canvasColor => canvasColor -> Canvas ()
shadowColor a1 = primitiveAttribute "shadowColor" [jsbCanvasColor a1]

-- | Sets the horizontal distance that a shadow will be offset (@0.0@ by default).
shadowOffsetX :: Double -> Canvas ()
shadowOffsetX a1 = primitiveAttribute "shadowOffsetX" [showJSB a1]

-- | Sets the vertical distance that a shadow will be offset (@0.0@ by default).
shadowOffsetY :: Double -> Canvas ()
shadowOffsetY a1 = primitiveAttribute "shadowOffsetY" [showJSB a1]

-- | Draws the current path's strokes with the current 'strokeStyle' ('black' by default).
--
-- ==== __Example__
--
-- @
-- 'rect'(10, 10, 100, 100)
-- 'stroke'()
-- @
stroke :: () -> Canvas ()
stroke () = primitiveMethod "stroke" []

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
strokeRect (a1,a2,a3,a4) = primitiveMethod "strokeRect"
  [showJSB a1, showJSB a2, showJSB a3, showJSB a4]

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
strokeStyle a1 = primitiveAttribute "strokeStyle"
  [jsbStyle a1]

-- | @'strokeText'(t, x, y)@ draws text @t@ (with no fill) at position @(x, y)@
-- using the current 'strokeStyle'.
--
-- ==== __Example__
--
-- @
-- 'font' \"48px serif\"
-- 'strokeText'(\"Hello, World!\", 50, 100)
-- @
strokeText :: (ST.Text, Double, Double) -> Canvas ()
strokeText (t, a, b) = primitiveMethod "strokeText"
  [showJSB t, showJSB a, showJSB b]

-- | Sets the 'TextAnchorAlignment' to use when drawing text.
textAlign :: TextAnchorAlignment -> Canvas ()
textAlign a1 = primitiveAttribute "textAlign" [showJSB a1]

-- | Sets the 'TextBaselineAlignment' to use when drawing text.
textBaseline :: TextBaselineAlignment -> Canvas ()
textBaseline a1 = primitiveAttribute "textBaseline" [showJSB a1]

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
transform (a1,a2,a3,a4,a5,a6) = primitiveMethod "transform"
  [showJSB a1, showJSB a2, showJSB a3, showJSB a4, showJSB a5, showJSB a6]

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
translate (a1,a2) = primitiveMethod "translate" [showJSB a1, showJSB a2]

-- | Change the canvas cursor to the specified URL or keyword.
--
-- ==== __Examples__
--
-- @
-- cursor $ 'url' \"image.png\" 'default_'
-- cursor 'crosshair'
-- @
cursor :: CanvasCursor cursor => cursor -> Canvas ()
cursor cur = primitiveAttribute "canvas.style.cursor" [jsbCanvasCursor cur]
