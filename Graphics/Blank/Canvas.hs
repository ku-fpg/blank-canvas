{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, ScopedTypeVariables, OverloadedStrings, FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.Canvas where

import           Control.Applicative
import           Control.Monad (ap, liftM2)
import           Control.Monad.IO.Class

import           Graphics.Blank.Events
import           Graphics.Blank.JavaScript

import           Data.Aeson (FromJSON(..),Value(..),encode)
import           Data.Aeson.Types (Parser, (.:))
import           Data.Char (chr)

import qualified Data.ByteString.Lazy as DBL
import           Data.Monoid
import qualified Data.Text as Text
import           Data.Text (Text)


data Canvas :: * -> * where
        Method    :: Method                      -> Canvas ()     -- <context>.<method>
        Command   :: Command                     -> Canvas ()     -- <command>
        Query     :: (Show a) => Query a         -> Canvas a
        With      :: CanvasContext -> Canvas a   -> Canvas a
        MyContext ::                                Canvas CanvasContext
        Bind      :: Canvas a -> (a -> Canvas b) -> Canvas b
        Return    :: a                           -> Canvas a
        LiftIO    :: IO a                        -> Canvas a
        ASync     ::                                Canvas ()

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

instance MonadIO Canvas where
  liftIO = LiftIO

-- HTML5 Canvas assignments: FillStyle, Font, GlobalAlpha, GlobalCompositeOperation, LineCap, LineJoin, LineWidth, MiterLimit, ShadowBlur, ShadowColor, ShadowOffsetX, ShadowOffsetY, StrokeStyle, TextAlign, TextBaseline
data Method
        -- regular HTML5 canvas commands
        = Arc (Float,Float,Float,Float,Float,Bool)
        | ArcTo (Float,Float,Float,Float,Float)
        | BeginPath
        | BezierCurveTo (Float,Float,Float,Float,Float,Float)
        | forall image . Image image => DrawImage (image,[Float]) -- 'drawImage' takes 2, 4 or 8 floats arguments
        | ClearRect (Float,Float,Float,Float)
        | Clip
        | ClosePath
        | Fill
        | FillRect (Float,Float,Float,Float)
        | forall style . Style style => FillStyle style
        | FillText (Text,Float,Float)
        | Font Text
        | GlobalAlpha Float
        | GlobalCompositeOperation Text
        | LineCap Text
        | LineJoin Text
        | LineTo (Float,Float)
        | LineWidth Float
        | MiterLimit Float
        | MoveTo (Float,Float)
        | PutImageData (ImageData,[Float])
        | QuadraticCurveTo (Float,Float,Float,Float)
        | Rect (Float,Float,Float,Float)
        | Restore
        | Rotate Float
        | Scale (Float,Float)
        | Save
        | SetTransform (Float,Float,Float,Float,Float,Float)
        | Stroke
        | StrokeRect (Float,Float,Float,Float)
        | StrokeText (Text,Float,Float)
        | forall style . Style style => StrokeStyle style
        | ShadowBlur Float
        | forall canvasColor . CanvasColor canvasColor => ShadowColor canvasColor
        | ShadowOffsetX Float
        | ShadowOffsetY Float
        | TextAlign Text
        | TextBaseline Text
        | Transform (Float,Float,Float,Float,Float,Float)
        | Translate (Float,Float)

data Command
  = Trigger Event
  | forall color . CanvasColor color => AddColorStop (Float, color) CanvasGradient
  | forall msg . JSArg msg => Log msg
  | Eval Text

instance Show Command where
  show (Trigger e) = "Trigger(" ++ map (chr . fromEnum) (DBL.unpack (encode e)) ++ ")"
  show (AddColorStop (off,rep) g)
     = showJS g ++ ".addColorStop(" ++ showJS off ++ "," ++ jsStyle rep ++ ")"
  show (Log msg) = "console.log(" ++ showJS msg ++ ")"
  show (Eval cmd) = Text.unpack cmd -- no escaping or interpretation

-----------------------------------------------------------------------------

-- | 'with' runs a set of canvas commands in the context
-- of a specific canvas buffer.
with :: CanvasContext -> Canvas a -> Canvas a
with = With

-- | 'myCanvasContext' returns the current 'CanvasContent'.
myCanvasContext :: Canvas CanvasContext
myCanvasContext = MyContext

-----------------------------------------------------------------------------

-- | trigger a specific named event, please.
trigger :: Event -> Canvas ()
trigger = Command . Trigger

-- | add a Color stop to a Canvas Gradient.
addColorStop :: CanvasColor color => (Float, color) -> CanvasGradient -> Canvas ()
addColorStop (off,rep) = Command . AddColorStop (off,rep)

-- | 'console_log' aids debugging by sending the argument to the browser console.log.
console_log :: JSArg msg => msg -> Canvas ()
console_log = Command . Log

-- | 'eval' executes the argument in JavaScript directly.
eval :: Text -> Canvas ()
eval = Command . Eval

-----------------------------------------------------------------------------
data Query :: * -> * where
        Device                                            :: Query DeviceAttributes
        ToDataURL                                         :: Query Text
        MeasureText          :: Text                      -> Query TextMetrics
        IsPointInPath        :: (Float,Float)             -> Query Bool
        NewImage             :: Text                      -> Query CanvasImage
        CreateLinearGradient :: (Float,Float,Float,Float)             -> Query CanvasGradient
        CreateRadialGradient :: (Float,Float,Float,Float,Float,Float) -> Query CanvasGradient
        CreatePattern        :: Image image => (image,Text) -> Query CanvasPattern
        NewCanvas            :: (Int,Int)                 -> Query CanvasContext
        GetImageData         :: (Float,Float,Float,Float) -> Query ImageData
        Sync                 ::                              Query ()

data DeviceAttributes = DeviceAttributes Int Int Float
        deriving Show

-- | The 'width' argument of 'TextMetrics' can trivially be projected out.
data TextMetrics = TextMetrics Float
        deriving Show

instance Show (Query a) where
  show Device                   = "Device"
  show ToDataURL                = "ToDataURL"
  show (MeasureText txt)        = "MeasureText(" ++ showJS txt ++ ")"
  show (IsPointInPath (x,y))    = "IsPointInPath(" ++ showJS x ++ "," ++ showJS y ++ ")"
  show (NewImage url)           = "NewImage(" ++ showJS url ++ ")"
  show (CreateLinearGradient (x0,y0,x1,y1)) = "CreateLinearGradient(" ++ showJS x0 ++ "," ++ showJS y0 ++ "," ++ showJS x1 ++ "," ++ showJS y1 ++ ")"
  show (CreateRadialGradient (x0,y0,r0,x1,y1,r1)) = "CreateRadialGradient(" ++ showJS x0 ++ "," ++ showJS y0 ++ "," ++ showJS r0 ++ "," ++ showJS x1 ++ "," ++ showJS y1 ++ "," ++ showJS r1 ++ ")"
  show (CreatePattern (img,str)) = "CreatePattern(" ++ jsImage img ++ "," ++ showJS str ++ ")"
  show (NewCanvas (x,y))         = "NewCanvas(" ++ showJS x ++ "," ++ showJS y ++ ")"
  show (GetImageData (sx,sy,sw,sh))
                                 = "GetImageData(" ++ showJS sx ++ "," ++ showJS sy ++ "," ++ showJS sw ++ "," ++ showJS sh ++ ")"
  show Sync                      = "Sync"

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Device {}) o    = uncurry3 DeviceAttributes <$> parseJSON o
parseQueryResult (ToDataURL {}) o = parseJSON o
parseQueryResult (MeasureText {}) (Object v) = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o        = parseJSON o
parseQueryResult (NewImage {}) o             = uncurry3 CanvasImage <$> parseJSON o
parseQueryResult (CreateLinearGradient {}) o = CanvasGradient <$> parseJSON o
parseQueryResult (CreateRadialGradient {}) o = CanvasGradient <$> parseJSON o
parseQueryResult (CreatePattern {}) o = CanvasPattern <$> parseJSON o
parseQueryResult (NewCanvas {}) o = uncurry3 CanvasContext <$> parseJSON o
parseQueryResult (GetImageData {}) (Object o) = ImageData
                                         <$> (o .: "width")
                                         <*> (o .: "height")
                                         <*> (o .: "data")
parseQueryResult (Sync {}) _ = return () -- we just accept anything; empty list sent
parseQueryResult _ _ = fail "no parse in blank-canvas server (internal error)"

uncurry3 :: (t0 -> t1 -> t2 -> t3) -> (t0, t1, t2) -> t3
uncurry3 f (a,b,c) = f a b c

device :: Canvas DeviceAttributes
device = Query Device

-- | Turn the canvas into a png data stream / data URL.
--
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: () -> Canvas Text
toDataURL () = Query ToDataURL

measureText :: Text -> Canvas TextMetrics
measureText = Query . MeasureText

isPointInPath :: (Float,Float) -> Canvas Bool
isPointInPath = Query . IsPointInPath

-- | 'image' takes a URL (perhaps a data URL), and returns the 'CanvasImage' handle,
-- _after_ loading.
-- The assumption is you are using local images, so loading should be near instant.
newImage :: Text -> Canvas CanvasImage
newImage = Query . NewImage

createLinearGradient :: (Float,Float,Float,Float) -> Canvas CanvasGradient
createLinearGradient = Query . CreateLinearGradient

createRadialGradient :: (Float,Float,Float,Float,Float,Float) -> Canvas CanvasGradient
createRadialGradient = Query . CreateRadialGradient

createPattern :: (CanvasImage, Text) -> Canvas CanvasPattern
createPattern = Query . CreatePattern

-- | Create a new, off-screen canvas buffer. Takes width and height.
newCanvas :: (Int,Int) -> Canvas CanvasContext
newCanvas = Query . NewCanvas

-- | Capture ImageDate from the Canvas.
getImageData :: (Float,Float,Float,Float) -> Canvas ImageData
getImageData = Query . GetImageData

-- | Send all commands to the browser, wait for the browser to ack, then continue.
sync :: Canvas ()
sync = Query $ Sync

-- | Send all commands to the browser, then continue without waiting.
async :: Canvas ()
async = ASync
