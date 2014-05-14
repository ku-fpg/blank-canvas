{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, ScopedTypeVariables, OverloadedStrings, FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.Canvas where

import Graphics.Blank.Events
import Graphics.Blank.JavaScript

import Data.Aeson (FromJSON(..),Value(..))
import Data.Aeson.Types (Parser, (.:))
import Data.List
import Control.Monad (ap)
import Control.Applicative
import Numeric


data Canvas :: * -> * where
        Method  :: Method                              -> Canvas ()     -- <context>.<method>
        Command :: Command                             -> Canvas ()     -- <command>
        Query   :: (Show a) => Query a                 -> Canvas a
        Bind    :: Canvas a -> (a -> Canvas b)         -> Canvas b
        Return  :: a                                   -> Canvas a

instance Monad Canvas where
        return = Return
        (>>=) = Bind

instance Applicative Canvas where
  pure  = return
  (<*>) = ap

instance Functor Canvas where
  fmap f c = c >>= return . f

-- HTML5 Canvas assignments: FillStyle, Font, GlobalAlpha, LineCap, LineJoin, LineWidth, MiterLimit, StrokeStyle, TextAlign, TextBaseline
data Method
        -- regular HTML5 canvas commands
        = Arc (Float,Float,Float,Float,Float,Bool)
        | ArcTo (Float,Float,Float,Float,Float)
        | BeginPath
        | BezierCurveTo (Float,Float,Float,Float,Float,Float)
        | forall image . Image image => DrawImage (image,[Float])
        | ClearRect (Float,Float,Float,Float)
        | Clip
        | ClosePath
        | Fill
        | FillRect (Float,Float,Float,Float)
        | forall style . Style style => FillStyle style
        | FillText (String,Float,Float)
        | Font String
        | GlobalAlpha Float
        | GlobalCompositeOperation String
        | LineCap String
        | LineJoin String
        | LineTo (Float,Float)
        | LineWidth Float
        | MiterLimit Float
        | MoveTo (Float,Float)
        | QuadraticCurveTo (Float,Float,Float,Float)
        | Rect (Float,Float,Float,Float)
        | Restore
        | Rotate Float
        | Scale (Float,Float)
        | Save
        | SetTransform (Float,Float,Float,Float,Float,Float)
        | Stroke
        | StrokeRect (Float,Float,Float,Float)
        | StrokeText (String,Float,Float)
        | StrokeStyle String
        | ShadowBlur Float
        | ShadowColor String
        | ShadowOffsetX Float
        | ShadowOffsetY Float
        | TextAlign String
        | TextBaseline String
        | Transform (Float,Float,Float,Float,Float,Float)
        | Translate (Float,Float)

data Command
  = Trigger NamedEvent
  | AddColorStop (Float,String) CanvasGradient

instance Show Command where
  show (Trigger (NamedEvent nm ev)) = "/* trigger */"
  show (AddColorStop (off,rep) g)
     = showJS g ++ ".addColorStop(" ++ showJS off ++ "," ++ showJS rep ++ ")"


-----------------------------------------------------------------------------

class JSArg a => Image a where
        
instance Image CanvasImage
--instance Image CanvasBuffer
-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

class JSArg a => Style a where

instance Style [Char]
instance Style CanvasGradient
instance Style CanvasPattern

-----------------------------------------------------------------------------

-- | trigger a specific named event, please.
trigger :: NamedEvent -> Canvas ()
trigger = Command . Trigger

-- | add a Color stop to a Canvas Gradient.
addColorStop :: (Float,String) -> CanvasGradient -> Canvas ()
addColorStop (off,rep) = Command . AddColorStop (off,rep)

-----------------------------------------------------------------------------
data Query :: * -> * where
        Size                         :: Query (Float,Float)
        ToDataURL                    :: Query String
        MeasureText :: String        -> Query TextMetrics
        IsPointInPath :: (Float,Float) -> Query Bool
        NewImage :: String             -> Query CanvasImage
        CreateLinearGradient :: [Float] -> Query CanvasGradient
        CreatePattern :: (CanvasImage,String) -> Query CanvasPattern

data TextMetrics = TextMetrics Float
        deriving Show

instance Show (Query a) where
  show Size                     = "reply(size(c))"
  show ToDataURL                = "reply(toDataURL(c))"
  show (MeasureText txt)        = "reply(c.measureText(" ++ showJS txt ++ "))"
  show (IsPointInPath (x,y))    = "reply(c.isPointInPath(" ++ showJS x ++ "," ++ showJS y ++ "))"
  show (NewImage url)           = "newImage(" ++ showJS url ++ ")"
  show (CreateLinearGradient fs) = "reply(gradients.push(c.createLinearGradient(" ++ showJS fs ++ ")) - 1)"
  show (CreatePattern (img,str)) = "reply(patterns.push(c.createPattern(" ++ showJS img ++ "," ++ showJS str ++ ")) - 1)"

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Size {}) o      = parseJSON o -- default is good
parseQueryResult (ToDataURL {}) o = parseJSON o
parseQueryResult (MeasureText {}) (Object v) = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o        = parseJSON o
parseQueryResult (NewImage {}) o             = CanvasImage <$> parseJSON o
parseQueryResult (CreateLinearGradient {}) o = CanvasGradient <$> parseJSON o
parseQueryResult (CreatePattern {}) o = CanvasPattern <$> parseJSON o
parseQueryResult _ _ = fail "no parse"

-- | size of the canvas
size :: Canvas (Float,Float)
size = Query Size

-- | Turn the canvas into a png data stream / data URL.
-- 
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: () -> Canvas String
toDataURL () = Query ToDataURL

measureText :: String -> Canvas TextMetrics
measureText = Query . MeasureText

isPointInPath :: (Float,Float) -> Canvas Bool
isPointInPath = Query . IsPointInPath

-- | 'image' takes a URL (perhaps a data URL), and returns the 'CanvasImage' handle, 
-- _after_ loading.
-- The assumption is you are using local images, so loading should be near instant.
newImage :: String -> Canvas CanvasImage
newImage = Query . NewImage 

createLinearGradient :: [Float] -> Canvas CanvasGradient
createLinearGradient = Query . CreateLinearGradient

createPattern :: (CanvasImage, String) -> Canvas CanvasPattern
createPattern = Query . CreatePattern


