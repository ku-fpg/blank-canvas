{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, ScopedTypeVariables, OverloadedStrings, FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.Canvas where

import           Control.Applicative
import           Control.Monad (ap, liftM2)

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
        Function  :: (Show a) => Function a      -> Canvas a
        Query     :: (Show a) => Query a         -> Canvas a
        With      :: CanvasContext -> Canvas a   -> Canvas a
        MyContext ::                                Canvas CanvasContext
        Bind      :: Canvas a -> (a -> Canvas b) -> Canvas b
        Return    :: a                           -> Canvas a

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
        = Arc (Double, Double, Double, Double, Double, Bool)
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
        | Font Text
        | GlobalAlpha Double
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
        | Rotate Double
        | Save
        | Scale (Double, Double)
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

data Command
  = Trigger Event
  | forall color . CanvasColor color => AddColorStop (Double, color) CanvasGradient
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
addColorStop :: CanvasColor color => (Double, color) -> CanvasGradient -> Canvas ()
addColorStop (off,rep) = Command . AddColorStop (off,rep)

-- | 'console_log' aids debugging by sending the argument to the browser console.log.
console_log :: JSArg msg => msg -> Canvas ()
console_log = Command . Log

-- | 'eval' executes the argument in JavaScript directly.
eval :: Text -> Canvas ()
eval = Command . Eval

-----------------------------------------------------------------------------

data Function :: * -> * where
  CreateLinearGradient :: (Double,Double,Double,Double)               -> Function CanvasGradient
  CreateRadialGradient :: (Double,Double,Double,Double,Double,Double) -> Function CanvasGradient

instance Show (Function a) where
  show (CreateLinearGradient (x0,y0,x1,y1)) = "createLinearGradient(" 
        ++ showJS x0 ++ "," ++ showJS y0 ++ "," ++ showJS x1 ++ "," 
        ++ showJS y1 ++ ")"
  show (CreateRadialGradient (x0,y0,r0,x1,y1,r1)) = "createRadialGradient(" 
        ++ showJS x0 ++ "," ++ showJS y0 ++ "," ++ showJS r0 ++ "," 
        ++ showJS x1 ++ "," ++ showJS y1 ++ "," ++ showJS r1 ++ ")"

-----------------------------------------------------------------------------

data Query :: * -> * where
        Device               ::                                            Query DeviceAttributes
        ToDataURL            ::                                            Query Text
        MeasureText          :: Text                                    -> Query TextMetrics
        IsPointInPath        :: (Double, Double)                        -> Query Bool
        NewImage             :: Text                                    -> Query CanvasImage
        CreatePattern        :: Image image => (image, RepeatDirection) -> Query CanvasPattern
        NewCanvas            :: (Int, Int)                              -> Query CanvasContext
        GetImageData         :: (Double, Double, Double, Double)        -> Query ImageData
        Sync                 ::                                            Query ()

data DeviceAttributes = DeviceAttributes Int Int Double
        deriving Show

-- | The 'width' argument of 'TextMetrics' can trivially be projected out.
data TextMetrics = TextMetrics Double
        deriving Show

instance Show (Query a) where
  show Device                       = "Device"
  show ToDataURL                    = "ToDataURL"
  show (MeasureText txt)            = "MeasureText(" ++ showJS txt ++ ")"
  show (IsPointInPath (x,y))        = "IsPointInPath(" ++ showJS x ++ "," ++ showJS y ++ ")"
  show (NewImage url)               = "NewImage(" ++ showJS url ++ ")"
  show (CreatePattern (img,dir))    = "CreatePattern(" ++ jsImage img ++ "," 
                                    ++ jsRepeatDirection dir ++ ")"
  show (NewCanvas (x,y))            = "NewCanvas(" ++ showJS x ++ "," ++ showJS y ++ ")"
  show (GetImageData (sx,sy,sw,sh)) = "GetImageData(" ++ showJS sx ++ "," ++ showJS sy 
                                   ++ "," ++ showJS sw ++ "," ++ showJS sh ++ ")"
  show Sync                         = "Sync"

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Device {}) o                = uncurry3 DeviceAttributes <$> parseJSON o
parseQueryResult (ToDataURL {}) o             = parseJSON o
parseQueryResult (MeasureText {}) (Object v)  = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o         = parseJSON o
parseQueryResult (NewImage {}) o              = uncurry3 CanvasImage <$> parseJSON o
parseQueryResult (CreatePattern {}) o         = CanvasPattern <$> parseJSON o
parseQueryResult (NewCanvas {}) o             = uncurry3 CanvasContext <$> parseJSON o
parseQueryResult (GetImageData {}) (Object o) = ImageData
                                           <$> (o .: "width")
                                           <*> (o .: "height")
                                           <*> (o .: "data")
parseQueryResult (Sync {}) _                  = return () -- we just accept anything; empty list sent
parseQueryResult _ _                          = fail "no parse in blank-canvas server (internal error)"

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

isPointInPath :: (Double, Double) -> Canvas Bool
isPointInPath = Query . IsPointInPath

-- | 'image' takes a URL (perhaps a data URL), and returns the 'CanvasImage' handle,
-- _after_ loading.
-- The assumption is you are using local images, so loading should be near instant.
newImage :: Text -> Canvas CanvasImage
newImage = Query . NewImage

createLinearGradient :: (Double, Double, Double, Double) -> Canvas CanvasGradient
createLinearGradient = Function . CreateLinearGradient

createRadialGradient :: (Double, Double, Double, Double, Double, Double) -> Canvas CanvasGradient
createRadialGradient = Function . CreateRadialGradient

createPattern :: (CanvasImage, RepeatDirection) -> Canvas CanvasPattern
createPattern = Query . CreatePattern

-- | Create a new, off-screen canvas buffer. Takes width and height.
newCanvas :: (Int, Int) -> Canvas CanvasContext
newCanvas = Query . NewCanvas

-- | Capture ImageDate from the Canvas.
getImageData :: (Double, Double, Double, Double) -> Canvas ImageData
getImageData = Query . GetImageData

-- | Send all commands to the browser, wait for the browser to ack, then continue.
sync :: Canvas ()
sync = Query $ Sync
