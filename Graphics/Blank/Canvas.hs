{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures, ScopedTypeVariables, OverloadedStrings #-}

module Graphics.Blank.Canvas where

import Graphics.Blank.Events

import Data.Aeson (FromJSON(..),Value(..))
import Data.Aeson.Types (Parser, (.:))
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
        | QuadraticCurveTo (Float,Float,Float,Float)
        | ClearRect (Float,Float,Float,Float)
        | Clip
        | ClosePath
        | Fill
        | FillRect (Float,Float,Float,Float)
        | FillStyle String
        | FillText (String,Float,Float)
        | Font String
        | GlobalAlpha Float
        | LineCap String
        | LineJoin String
        | LineTo (Float,Float)
        | LineWidth Float
        | MiterLimit Float
        | MoveTo (Float,Float)
        | Rect (Float,Float,Float,Float)
        | Restore
        | Rotate Float
        | Scale (Float,Float)
        | Save
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
        | Trigger EventName Event


data Query :: * -> * where
        Size                         :: Query (Float,Float)
        ToDataURL                    :: Query String
        MeasureText :: String        -> Query TextMetrics
        IsPointInPath :: (Float,Float) -> Query Bool

data TextMetrics = TextMetrics Float
        deriving Show

instance Show (Query a) where
  show Size      = "size(c)"
  show ToDataURL = "toDataURL(c)"
  show (MeasureText txt) = "c.measureText(" ++ show txt ++ ")"
  show (IsPointInPath (x,y)) = "c.isPointInPath(" ++ showJ x ++ "," ++ showJ y ++ ")"

-- This is how we take our value to bits
parseQueryResult :: Query a -> Value -> Parser a
parseQueryResult (Size {}) o      = parseJSON o -- default is good
parseQueryResult (ToDataURL {}) o = parseJSON o
parseQueryResult (MeasureText {}) (Object v) = TextMetrics <$> v .: "width"
parseQueryResult (IsPointInPath {}) o          = parseJSON o
parseQueryResult _ _ = fail "no parse"

-- | size of the canvas
size :: Canvas (Float,Float)
size = Query Size

-- | Turn the canvas into a png data stream.
-- 
-- > "data:image/png;base64,iVBORw0KGgo.."
--
toDataURL :: Canvas String
toDataURL = Query ToDataURL

measureText :: String -> Canvas TextMetrics
measureText = Query . MeasureText

isPointInPath :: (Float,Float) -> Canvas Bool
isPointInPath = Query . IsPointInPath

showJ :: Float -> String
showJ a = showFFloat (Just 3) a ""

showB :: Bool -> String
showB True = "true"
showB False = "false"


-- | trigger a specific event, please.
trigger :: EventName -> Event -> Canvas ()
trigger nm ev = Command (Trigger nm ev)

{-
-- | read a specific event; wait for it if the event is not in queue.
-- **Thows away all other events while waiting.**
readEvent :: EventName -> Canvas Event
readEvent nm = fmap (\ (NamedEvent _ e) -> e) (readEvents [nm])

-- | read a specific set of events; wait for it if the event/events is not in queue.
-- **Throws away all other non-named events while waiting.**
readEvents :: [EventName] -> Canvas NamedEvent
readEvents nms = Get nms $ \ q -> do
   let loop = do ne@(NamedEvent n _) <- readEventQueue q
                 if n `elem` nms
                 then return ne -- return if the event is one of the approved list
                 else loop
   loop

-- | read a specific event. **Throws away all events not named**
tryReadEvent :: EventName -> Canvas (Maybe Event)
tryReadEvent nm = fmap (fmap (\ (NamedEvent _ e) -> e)) (tryReadEvents [nm])

-- | read a specific set of events. **Throws away all non-named events while waiting.**
tryReadEvents :: [EventName] -> Canvas (Maybe NamedEvent)
tryReadEvents nms = Get nms $ \ q -> do
   let loop = do opt <- tryReadEventQueue q
                 case opt of
                        -- return if the event is one of the approved list
                   Just (NamedEvent n _)
                        | n `elem` nms -> return opt
                        | otherwise    -> loop
                   Nothing -> return Nothing
   loop


-}