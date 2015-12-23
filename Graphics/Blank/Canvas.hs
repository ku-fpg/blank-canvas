{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Graphics.Blank.Canvas where

import           Control.Monad (ap, liftM2)

import           Data.Aeson (FromJSON(..),Value(..),encode)
import           Data.Aeson.Types (Parser, (.:))
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy.Encoding (decodeUtf8)

import           Graphics.Blank.Events
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Types.Cursor
import           Graphics.Blank.Types.Font

import           Prelude.Compat

import           TextShow
import           TextShow.TH (deriveTextShow)

data DeviceAttributes = DeviceAttributes Int Int Double deriving Show
$(deriveTextShow ''DeviceAttributes)

-- | The 'width' argument of 'TextMetrics' can trivially be projected out.
data TextMetrics = TextMetrics Double deriving Show
$(deriveTextShow ''TextMetrics)

-----------------------------------------------------------------------------

data Canvas :: * -> * where
        Method      :: Method                      -> Canvas ()     -- <context>.<method>
        Command     :: Command                     -> Canvas ()     -- <command>
        Function    :: TextShow a => Function a    -> Canvas a
        Query       :: TextShow a => Query a       -> Canvas a
        With        :: CanvasContext -> Canvas a   -> Canvas a
        MyContext   ::                                Canvas CanvasContext
        Bind        :: Canvas a -> (a -> Canvas b) -> Canvas b
        MethodAudio :: MethodAudio                 -> Canvas ()     -- <audiofile>.<method>        
        Return      :: a                           -> Canvas a

instance Monad Canvas where
        return = Return
        (>>=) = Bind

instance Applicative Canvas where
  pure  = return
  (<*>) = ap

instance Functor Canvas where
  fmap f c = c >>= return . f

instance Monoid a => Monoid (Canvas a) where
  mappend = liftM2 mappend
  mempty  = return mempty

-- HTML5 Canvas assignments: FillStyle, Font, GlobalAlpha, GlobalCompositeOperation, LineCap, LineJoin, LineWidth, MiterLimit, ShadowBlur, ShadowColor, ShadowOffsetX, ShadowOffsetY, StrokeStyle, TextAlign, TextBaseline
data Method
        -- regular HTML5 canvas commands
        = Arc (Double, Double, Double, Radians, Radians, Bool)
        | ArcTo (Double, Double, Double, Double, Double)
        | BeginPath
        | BezierCurveTo (Double, Double, Double, Double, Double, Double)
        | ClearRect (Double, Double, Double, Double)
        | Clip
        | ClosePath
        | forall image . Image image => DrawImage (image,[Double]) -- drawImage' takes 2, 4, or 8 'Double' arguments. See 'drawImageAt', 'drawImageSize', and 'drawImageCrop' for variants with exact numbers of arguments.
        | Fill
        | FillRect (Double, Double, Double, Double)
        | forall style . Style style => FillStyle style
        | FillText (Text, Double, Double)
        | forall canvasFont . CanvasFont canvasFont => Font canvasFont
        | GlobalAlpha Alpha
        | GlobalCompositeOperation Text
        | LineCap LineEndCap
        | LineJoin LineJoinCorner
        | LineTo (Double, Double)
        | LineWidth Double
        | MiterLimit Double
        | MoveTo (Double, Double)
        | PutImageData (ImageData, [Double]) -- 'putImageData' takes 2 or 6 'Double' arguments. See `putImageDataAt' and `putImageDataDirty' for variants with exact numbers of arguments.
        | QuadraticCurveTo (Double, Double, Double, Double)
        | Rect (Double, Double, Double, Double)
        | Restore
        | Rotate Radians
        | Save
        | Scale (Interval, Interval)
        | SetTransform (Double, Double, Double, Double, Double, Double)
        | ShadowBlur Double
        | forall canvasColor . CanvasColor canvasColor => ShadowColor canvasColor
        | ShadowOffsetX Double
        | ShadowOffsetY Double
        | Stroke
        | StrokeRect (Double, Double, Double, Double)
        | forall style . Style style => StrokeStyle style
        | StrokeText (Text,Double, Double)
        | TextAlign TextAnchorAlignment
        | TextBaseline TextBaselineAlignment
        | Transform (Double, Double, Double, Double, Double, Double)
        | Translate (Double, Double)

-- Audio object methods: play(), pause(), setVolume()
data MethodAudio
        = forall audio . Audio audio => PlayAudio             audio
        | forall audio . Audio audio => PauseAudio            audio
        | forall audio . Audio audio => SetCurrentTimeAudio  (audio, Double)
        | forall audio . Audio audio => SetLoopAudio         (audio, Bool)
        | forall audio . Audio audio => SetMutedAudio        (audio, Bool)
        | forall audio . Audio audio => SetPlaybackRateAudio (audio, Double)
        | forall audio . Audio audio => SetVolumeAudio       (audio, Double)          

data Command
  = Trigger Event
  | forall color . CanvasColor color => AddColorStop (Interval, color) CanvasGradient
  | forall msg . JSArg msg => Log msg
  | Eval Text

instance Show Command where
  showsPrec p = showsPrec p . FromTextShow

instance TextShow Command where
  showb (Trigger e) = "Trigger(" <> (fromLazyText . decodeUtf8 $ encode e) <> singleton ')'
  showb (AddColorStop (off,rep) g) = jsCanvasGradient g <> ".addColorStop("
         <> jsDouble off <> singleton ',' <> jsCanvasColor rep
         <> singleton ')'
  showb (Log msg) = "console.log(" <> showbJS msg <> singleton ')'
  showb (Eval cmd) = fromText cmd -- no escaping or interpretation

-----------------------------------------------------------------------------

-- | 'with' runs a set of canvas commands in the context
-- of a specific canvas buffer.
with :: CanvasContext -> Canvas a -> Canvas a
with = With

-- | 'myCanvasContext' returns the current 'CanvasContext'.
myCanvasContext :: Canvas CanvasContext
myCanvasContext = MyContext

-----------------------------------------------------------------------------

-- | Triggers a specific named event.
trigger :: Event -> Canvas ()
trigger = Command . Trigger

-- | Adds a color and stop position in a 'CanvasGradient'. A stop position is a
-- number between 0.0 and 1.0 that represents the position between start and stop
-- in a gradient.
--
-- ==== __Example__
--
-- @
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- grd # 'addColorStop'(0, 'red')
-- @
addColorStop :: CanvasColor color => (Interval, color) -> CanvasGradient -> Canvas ()
addColorStop (off,rep) = Command . AddColorStop (off,rep)

-- | 'console_log' aids debugging by sending the argument to the browser @console.log@.
console_log :: JSArg msg => msg -> Canvas ()
console_log = Command . Log

-- | 'eval' executes the argument in JavaScript directly.
eval :: Text -> Canvas ()
eval = Command . Eval

-----------------------------------------------------------------------------

data Function :: * -> * where
  CreateLinearGradient :: (Double,Double,Double,Double)               -> Function CanvasGradient
  CreateRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Function CanvasGradient
  CreatePattern        :: Image image => (image, RepeatDirection)     -> Function CanvasPattern


instance Show (Function a) where
  showsPrec p = showsPrec p . FromTextShow

instance TextShow (Function a) where
  showb (CreateLinearGradient (x0,y0,x1,y1)) = "createLinearGradient("
        <> jsDouble x0 <> singleton ',' <> jsDouble y0 <> singleton ','
        <> jsDouble x1 <> singleton ',' <> jsDouble y1 <> singleton ')'
  showb (CreateRadialGradient (x0,y0,r0,x1,y1,r1)) = "createRadialGradient("
        <> jsDouble x0 <> singleton ',' <> jsDouble y0 <> singleton ',' <> jsDouble r0 <> singleton ','
        <> jsDouble x1 <> singleton ',' <> jsDouble y1 <> singleton ',' <> jsDouble r1 <> singleton ')'
  showb (CreatePattern (img,dir)) = "createPattern("
        <> jsImage img <> singleton ',' <> jsRepeatDirection dir <> singleton ')'

-----------------------------------------------------------------------------

data Query :: * -> * where
        Device               ::                                            Query DeviceAttributes
        ToDataURL            ::                                            Query Text
        MeasureText          :: Text                                    -> Query TextMetrics
        IsPointInPath        :: (Double, Double)                        -> Query Bool
        NewImage             :: Text                                    -> Query CanvasImage
        NewAudio             :: Text                                    -> Query CanvasAudio
        NewCanvas            :: (Int, Int)                              -> Query CanvasContext
        GetImageData         :: (Double, Double, Double, Double)        -> Query ImageData
        Cursor               :: CanvasCursor cursor => cursor           -> Query ()
        Sync                 ::                                            Query ()
        CurrentTimeAudio     :: CanvasAudio                             -> Query Double
        -- GetVolumeAudio       :: CanvasAudio                             -> Query Double

instance Show (Query a) where
  showsPrec p = showsPrec p . FromTextShow

instance TextShow (Query a) where
  showb Device                       = "Device"
  showb ToDataURL                    = "ToDataURL"
  showb (MeasureText txt)            = "MeasureText(" <> jsText txt <> singleton ')'
  showb (IsPointInPath (x,y))        = "IsPointInPath(" <> jsDouble x <> singleton ','
                                                        <> jsDouble y <> singleton ')'
  showb (NewImage url')              = "NewImage(" <> jsText url' <> singleton ')'
  showb (NewAudio txt)               = "NewAudio(" <> jsText txt  <> singleton ')'

  showb (NewCanvas (x,y))            = "NewCanvas(" <> jsInt x <> singleton ','
                                                    <> jsInt y <> singleton ')'
  showb (GetImageData (sx,sy,sw,sh)) = "GetImageData(" <> jsDouble sx <> singleton ','
                                                       <> jsDouble sy <> singleton ','
                                                       <> jsDouble sw <> singleton ','
                                                       <> jsDouble sh <> singleton ')'
  showb (Cursor cur)                 = "Cursor(" <> jsCanvasCursor cur <> singleton ')'
  showb Sync                         = "Sync"
  showb (CurrentTimeAudio aud)       = "CurrentTimeAudio(" <> jsIndexAudio aud <> singleton ')'
  -- showb (GetVolumeAudio   aud)       = "GetVolumeAudio("   <> jsIndexAudio aud <> singleton ')'

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Device {}) o                = uncurry3 DeviceAttributes <$> parseJSON o
parseQueryResult (ToDataURL {}) o             = parseJSON o
parseQueryResult (MeasureText {}) (Object v)  = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o         = parseJSON o
parseQueryResult (NewImage {}) o              = uncurry3 CanvasImage <$> parseJSON o
parseQueryResult (NewAudio {}) o              = uncurry CanvasAudio <$> parseJSON o
parseQueryResult (NewCanvas {}) o             = uncurry3 CanvasContext <$> parseJSON o
parseQueryResult (GetImageData {}) (Object o) = ImageData
                                           <$> (o .: "width")
                                           <*> (o .: "height")
                                           <*> (o .: "data")
parseQueryResult (Cursor {}) _                = return ()
parseQueryResult (Sync {}) _                  = return () -- we just accept anything; empty list sent
parseQueryResult (CurrentTimeAudio {}) o      = parseJSON o
-- parseQueryResult (GetVolumeAudio   {}) 
parseQueryResult _ _                          = fail "no parse in blank-canvas server (internal error)"

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

device :: Canvas DeviceAttributes
device = Query Device

-- | Turn the canvas into a PNG data stream / data URL.
--
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: () -> Canvas Text
toDataURL () = Query ToDataURL

-- | Queries the measured width of the text argument.
--
-- ==== __Example__
--
-- @
-- 'TextMetrics' w <- 'measureText' \"Hello, World!\"
-- @
measureText :: Text -> Canvas TextMetrics
measureText = Query . MeasureText

-- | @'isPointInPath'(x, y)@ queries whether point @(x, y)@ is within the current path.
--
-- ==== __Example__
--
-- @
-- 'rect'(10, 10, 100, 100)
-- 'stroke'()
-- b <- 'isPointInPath'(10, 10) -- b == True
-- @
isPointInPath :: (Double, Double) -> Canvas Bool
isPointInPath = Query . IsPointInPath

-- | 'newImage' takes a URL (perhaps a data URL), and returns the 'CanvasImage' handle
-- /after/ loading.
-- If you are using local images, loading should be near instant.
newImage :: Text -> Canvas CanvasImage
newImage = Query . NewImage

-- | 'newAudio' takes a URL (or file path) to an audio file and returns the 'CanvasAudio' handle
-- /after/ loading.
-- If you are using local audio files, loading should be near instant.
newAudio :: Text -> Canvas CanvasAudio
newAudio = Query . NewAudio

-- | 'currentTimeAudio' returns the current time (in seconds) of the audio playback of
-- the specified CanvasAudio.
-- Example:
--
-- @
-- aud <- 'newAudio' \"bach_invention.wav\"
-- 'playAudio' aud
-- cur <- 'currentTimeAudio' aud
-- @

currentTimeAudio :: CanvasAudio -> Canvas Double
currentTimeAudio = Query . CurrentTimeAudio

-- | @'createLinearGradient'(x0, y0, x1, y1)@ creates a linear gradient along a line,
-- which can be used to fill other shapes.
--
-- * @x0@ is the starting x-coordinate of the gradient
--
-- * @y0@ is the starting y-coordinate of the gradient
--
-- * @x1@ is the ending y-coordinate of the gradient
--
-- * @y1@ is the ending y-coordinate of the gradient
--
-- ==== __Example__
--
-- @
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- grd # 'addColorStop'(0, \"blue\")
-- grd # 'addColorStop'(1, \"red\")
-- 'fillStyle' grd
-- @

createLinearGradient :: (Double, Double, Double, Double) -> Canvas CanvasGradient
createLinearGradient = Function . CreateLinearGradient

-- | @'createRadialGradient'(x0, y0, r0, x1, y1, r1)@ creates a radial gradient given
-- by the coordinates of two circles, which can be used to fill other shapes.
--
-- * @x0@ is the x-axis of the coordinate of the start circle
--
-- * @y0@ is the y-axis of the coordinate of the start circle
--
-- * @r0@ is the radius of the start circle
--
-- * @x1@ is the x-axis of the coordinate of the end circle
--
-- * @y1@ is the y-axis of the coordinate of the end circle
--
-- * @r1@ is the radius of the end circle
--
-- ==== __Example__
--
-- @
-- grd <- 'createRadialGradient'(100,100,100,100,100,0)
-- grd # 'addColorStop'(0, \"blue\")
-- grd # 'addColorStop'(1, \"red\")
-- 'fillStyle' grd
-- @
createRadialGradient :: (Double, Double, Double, Double, Double, Double) -> Canvas CanvasGradient
createRadialGradient = Function . CreateRadialGradient

-- | Creates a pattern using a 'CanvasImage' and a 'RepeatDirection'.
--
-- ==== __Example__
--
-- @
-- img <- newImage \"cat.jpg\"
-- pat <- 'createPattern'(img, 'repeatX')
-- 'fillStyle' pat
-- @
createPattern :: (CanvasImage, RepeatDirection) -> Canvas CanvasPattern
createPattern = Function . CreatePattern

-- | Create a new, off-screen canvas buffer. Takes width and height as arguments.
newCanvas :: (Int, Int) -> Canvas CanvasContext
newCanvas = Query . NewCanvas

-- | @'getImageData'(x, y, w, h)@ capture 'ImageData' from the rectangle with
-- upper-left corner @(x, y)@, width @w@, and height @h@.
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = Query . GetImageData

-- | Change the canvas cursor to the specified URL or keyword. 
--
-- ==== __Examples__
--
-- @
-- cursor $ 'url' \"image.png\" 'default_'
-- cursor 'crosshair'
-- @
cursor :: CanvasCursor cursor => cursor -> Canvas ()
cursor = Query . Cursor

-- | Send all commands to the browser, wait for the browser to act, then continue.
sync :: Canvas ()
sync = Query Sync
