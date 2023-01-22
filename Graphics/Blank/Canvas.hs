{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Blank.Canvas where


import           Data.Aeson (FromJSON(..),Value(..), Result(..), encode, withObject)
import           Data.Aeson.Types (Parser, parse, (.:))
import           Data.Text.Lazy (Text, fromStrict, toStrict)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text as ST

import           Graphics.Blank.Events
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Types.Font

import           Graphics.Blank.Instr
import qualified Graphics.Blank.Instr as I

import           Prelude.Compat

import           TextShow.TH (deriveTextShow)

import           Control.Monad (liftM2)
--import           Control.Remote.Monad hiding (primitive)
--import qualified Control.Remote.Monad as RM

import qualified Network.JavaScript as JS


data DeviceAttributes = DeviceAttributes Int Int Double deriving Show
$(deriveTextShow ''DeviceAttributes)

instance InstrShow DeviceAttributes

-- | The 'width' argument of 'TextMetrics' can trivially be projected out.
data TextMetrics = TextMetrics Double deriving Show
$(deriveTextShow ''TextMetrics)

instance InstrShow TextMetrics

-----------------------------------------------------------------------------

newtype Canvas a = Canvas
        (CanvasContext          -- the context, for the graphic contexts
         -> JS.RemoteMonad a)

instance Functor Canvas where
  fmap f (Canvas g) = Canvas $ \ cc -> fmap f (g cc)

instance Applicative Canvas where
  pure a = Canvas $ \ _ -> return a
  Canvas f <*> Canvas g = Canvas $ \ cc -> (f cc <*> g cc)
  Canvas f *> Canvas g = Canvas $ \ cc -> f cc *> g cc
  Canvas f <* Canvas g = Canvas $ \ cc -> f cc <* g cc

instance Monad Canvas where
  return = pure
  Canvas m >>= k = Canvas $ \ cc -> do
   r <- m cc
   case k r of
     Canvas m' -> m' cc
  (>>) = (*>)

instance Semigroup a => Semigroup (Canvas a) where
  (<>) = liftM2 (<>)

instance Monoid a => Monoid (Canvas a) where
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftM2 mappend
#endif
  mempty  = return mempty

-- Not sure about this overloading
class Method r where
  type Context r
  context :: (JSArg (Context r) => Context r -> JS.RemoteMonad ()) -> r

instance Method (Canvas ()) where
  type Context (Canvas ()) = CanvasContext
  context f = Canvas f

instance JSArg c => Method (c -> Canvas ()) where
  type Context (c -> Canvas ()) = c
  context f c = Canvas $ \ _ -> f c

primitiveMethod :: Method m => JS.JavaScript -> [JS.JavaScript] -> m
primitiveMethod f args = context $ \ cc ->
  JS.command $ showJSB cc <> "." <> JS.call f args

-- A bit of a hack; we use method generation to also
-- generate attribute assignment.
primitiveAttribute :: Method m => JS.JavaScript -> [JS.JavaScript] -> m
primitiveAttribute f args = primitiveMethod (f <> "=") args

primitiveCommand :: JS.JavaScript -> [JS.JavaScript] -> Canvas ()
primitiveCommand f args = Canvas $ \ _ ->
  JS.command $ JS.call f args

primitiveConstructor :: JS.JavaScript -> [JS.JavaScript]
         	     -> Canvas (JS.RemoteValue a)
primitiveConstructor f args = Canvas $ \ cc ->
  JS.constructor $ showJSB cc <> "." <> JS.call f args

blankConstructor :: JS.JavaScript -> [JS.JavaScript]
                   -> Canvas (JS.RemoteValue a)
blankConstructor f args = Canvas $ \ cc ->
  JS.constructor $ JS.call (JS.call f args) [showJSB cc]

primitiveQuery :: JS.JavaScript
	       -> [JS.JavaScript]
	       -> (Value -> Parser a)
	       -> Canvas a
primitiveQuery f args k = Canvas $ \ cc ->
  (\ v -> case parse k v of
    Error msg -> error msg -- TODO: revisit this fail
    Success a -> a) <$> (JS.procedure $
       (JS.call f args <> "(" <> showJSB cc <> ")"))


{-
-- Audio object methods: play(), pause(), setVolume()
data MethodAudio
        = forall audio . Audio audio => PlayAudio             audio
        | forall audio . Audio audio => PauseAudio            audio
        | forall audio . Audio audio => SetCurrentTimeAudio  (audio, Double)
        | forall audio . Audio audio => SetLoopAudio         (audio, Bool)
        | forall audio . Audio audio => SetMutedAudio        (audio, Bool)
        | forall audio . Audio audio => SetPlaybackRateAudio (audio, Double)
        | forall audio . Audio audio => SetVolumeAudio       (audio, Double)
-}

-----------------------------------------------------------------------------

-- | 'with' runs a set of canvas commands in the context
-- of a specific canvas buffer.
with :: CanvasContext -> Canvas a -> Canvas a
with c (Canvas m) = Canvas $ \ _ -> m c

-- | 'myCanvasContext' returns the current 'CanvasContext'.
myCanvasContext :: Canvas CanvasContext
myCanvasContext = Canvas $ return

-----------------------------------------------------------------------------

-- | Triggers a specific named event.
trigger :: Event -> Canvas ()
trigger ev = primitiveCommand "Trigger"
  [JS.JavaScript $ decodeUtf8 $ encode ev]

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
addColorStop (off,rep) = primitiveMethod "addColorStop"
  [showJSB off, jsbCanvasColor rep]

-- | 'console_log' aids debugging by sending the argument to the browser @console.log@.
console_log :: JSArg msg => msg -> Canvas ()
console_log msg = primitiveCommand "console.log" [showJSB msg]

-- | 'eval' executes the argument in JavaScript directly.
eval :: ST.Text -> Canvas ()
eval txt = Canvas $ \ _ -> JS.command $ JS.JavaScript $ fromStrict txt

-----------------------------------------------------------------------------

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

device :: Canvas DeviceAttributes
device = primitiveQuery "Device" [] $ \ v ->
  uncurry3 DeviceAttributes <$> parseJSON v

-- | Turn the canvas into a PNG data stream / data URL.
--
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: () -> Canvas ST.Text
toDataURL () = primitiveQuery "ToDataURL" [] parseJSON

-- | Queries the measured width of the text argument.
--
-- ==== __Example__
--
-- @
-- 'TextMetrics' w <- 'measureText' \"Hello, World!\"
-- @
measureText :: ST.Text -> Canvas TextMetrics
measureText txt = primitiveQuery "MeasureText" [showJSB txt] $
  withObject "TextMetrics" $ \ o ->
    TextMetrics <$> o .: "width"

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
isPointInPath (a1,a2) =
  primitiveQuery "IsPointInPath" [showJSB a1,showJSB a2] parseJSON

-- | 'newImage' takes a URL (perhaps a data URL), and returns the 'CanvasImage' handle
-- /after/ loading.
-- If you are using local images, loading should be near instant.
newImage :: URL -> Canvas CanvasImage
newImage (URL txt) = primitiveQuery "NewImage" [showJSB txt] $ \ v ->
  uncurry3 CanvasImage <$> parseJSON v

-- | 'newAudio' takes a URL (or file path) to an audio file and returns the 'CanvasAudio' handle
-- /after/ loading.
-- If you are using local audio files, loading should be near instant.
-- TODO
--newAudio :: URL -> Canvas CanvasAudio
--newAudio (URL txt) = primitiveQuery "NewAudio" [showJSB txt] $ \ v ->
--  uncurry CanvasAudio <$> parseJSON v

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
currentTimeAudio = error "TODO" -- primitive . Query . CurrentTimeAudio

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
createLinearGradient (x0,y0,x1,y1) =
  CanvasGradient <$> primitiveConstructor "createLinearGradient"
    [showJSB x0,showJSB y0,showJSB x1,showJSB y1]

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
createRadialGradient (x0,y0,r0,x1,y1,r1) =
  CanvasGradient <$> primitiveConstructor "createRadialGradient"
    [showJSB x0,showJSB y0,showJSB r0,showJSB x1,showJSB y1,showJSB r1]

-- | Creates a pattern using a 'CanvasImage' and a 'RepeatDirection'.
--
-- ==== __Example__
--
-- @
-- url <- static \"cat.jpg\"
-- ...
-- send ... $ do
--    img <- newImage \"cat.jpg\"
--    pat <- 'createPattern'(img, 'repeatX')
--    'fillStyle' pat
-- @
createPattern :: (CanvasImage, RepeatDirection) -> Canvas CanvasPattern
createPattern (img, dir) =
  CanvasPattern <$> primitiveConstructor "createPattern"
    [jsbImage img, showJSB dir]

-- | Create a new, off-screen canvas buffer. Takes width and height as arguments.
newCanvas :: (Int, Int) -> Canvas CanvasContext
newCanvas (w,h) =
  (\ cc -> CanvasContext (Just cc) w h) <$>
    blankConstructor "NewCanvas" [showJSB w, showJSB h]

-- | @'getImageData'(x, y, w, h)@ capture 'ImageData' from the rectangle with
-- upper-left corner @(x, y)@, width @w@, and height @h@.
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData (sx,sy,sw,sh) = primitiveQuery "GetImageData"
  [showJSB sx, showJSB sy, showJSB sw, showJSB sh] $
    withObject "ImageData" $ \ o ->
      ImageData <$> (o .: "width")
                <*> (o .: "height")
                <*> (o .: "data")

-- | Send all commands to the browser, wait for the browser to act, then continue.
sync :: Canvas ()
sync = primitiveQuery "Sync" [] $ const $ return ()

