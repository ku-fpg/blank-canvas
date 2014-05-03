{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, ScopedTypeVariables, OverloadedStrings #-}

module Graphics.Blank.Canvas where

import Graphics.Blank.Events

import Data.Aeson (FromJSON(..),Value(..))
import Data.Aeson.Types (Parser, (.:))
import Data.List
import Control.Monad (ap)
import Control.Applicative
import Numeric


data Canvas :: * -> * where
        Command :: Command                             -> Canvas ()
        Query   :: (Show a) => Query a                 -> Canvas a
        Bind    :: Canvas a -> (a -> Canvas b)         -> Canvas b
        Return  :: a                                   -> Canvas a
--        Size    ::                                        Canvas (Float,Float)

instance Monad Canvas where
        return = Return
        (>>=) = Bind

instance Applicative Canvas where
  pure  = return
  (<*>) = ap

instance Functor Canvas where
  fmap f c = c >>= return . f

-- HTML5 Canvas assignments: FillStyle, Font, GlobalAlpha, LineCap, LineJoin, LineWidth, MiterLimit, StrokeStyle, TextAlign, TextBaseline
data Command
        -- regular HTML5 canvas commands
        = Arc (Float,Float,Float,Float,Float,Bool)
        | ArcTo (Float,Float,Float,Float,Float)
        | BeginPath
        | BezierCurveTo (Float,Float,Float,Float,Float,Float)
        | DrawImage (Image,[Float])
        | ClearRect (Float,Float,Float,Float)
        | Clip
        | ClosePath
        | Fill
        | FillRect (Float,Float,Float,Float)
        | FillStyle String
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
        -- Specials
        | Trigger NamedEvent

data Query :: * -> * where
        Size                         :: Query (Float,Float)
        ToDataURL                    :: Query String
        MeasureText :: String        -> Query TextMetrics
        IsPointInPath :: (Float,Float) -> Query Bool
        NewImage :: String           -> Query Image

data TextMetrics = TextMetrics Float
        deriving Show

-- A handle to the image. Images can not be destroyed.
data Image = Image Int deriving (Show,Eq,Ord)

instance Show (Query a) where
  show Size                     = "reply(size(c))"
  show ToDataURL                = "reply(toDataURL(c))"
  show (MeasureText txt)        = "reply(c.measureText(" ++ showS txt ++ "))"
  show (IsPointInPath (x,y))    = "reply(c.isPointInPath(" ++ showJ x ++ "," ++ showJ y ++ "))"
  show (NewImage url)           = "newImage(" ++ showS url ++ ")"

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Size {}) o      = parseJSON o -- default is good
parseQueryResult (ToDataURL {}) o = parseJSON o
parseQueryResult (MeasureText {}) (Object v) = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o          = parseJSON o
parseQueryResult (NewImage {}) o             = Image <$> parseJSON o
parseQueryResult _ _ = fail "no parse"

-- | size of the canvas
size :: Canvas (Float,Float)
size = Query Size

-- | Turn the canvas into a png data stream / data URL.
-- 
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: Canvas String
toDataURL = Query ToDataURL

measureText :: String -> Canvas TextMetrics
measureText = Query . MeasureText

isPointInPath :: (Float,Float) -> Canvas Bool
isPointInPath = Query . IsPointInPath

-- | 'image' takes a URL (perhaps a data URL), and returns the Image handle, after loading.
-- The assumption is you are using local images, so loading should be near instant.
newImage :: String -> Canvas Image
newImage = Query . NewImage

-- ? Remove the reference to the Image. After this, the image can not be reloaded.
--freeImage :: Image -> Canvas ()
--freeImage = undefined


----------------------------------------------------------------

showJ :: Float -> String
showJ a = showFFloat (Just 3) a ""

showJs :: [Float] -> String
showJs fs = concat $ intersperse "," [ showFFloat (Just 3) a "" | a <- fs ]


showB :: Bool -> String
showB True = "true"
showB False = "false"

showI :: Image -> String
showI (Image n) = "images[" ++ show n ++ "]"

showS :: String -> String
showS = show

-- | trigger a specific named event, please.
trigger :: NamedEvent -> Canvas ()
trigger = Command . Trigger
