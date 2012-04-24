{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank.Canvas where

import Graphics.Blank.Events

import Control.Applicative (Applicative(..))
import Control.Monad (ap)
import Numeric

data Canvas :: * -> * where
        Command :: Command                           -> Canvas ()
        Bind    :: Canvas a -> (a -> Canvas b)       -> Canvas b
        Return  :: a                                 -> Canvas a
        Get     :: EventName -> (EventQueue -> IO a) -> Canvas a
        Size    ::                                      Canvas (Float,Float)

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
        | BeginPath
        | BezierCurveTo (Float,Float,Float,Float,Float,Float)
        | ClearRect (Float,Float,Float,Float)
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
        | Restore
        | Rotate Float
        | Scale (Float,Float)
        | Save
        | Stroke
        | StrokeRect (Float,Float,Float,Float)
        | StrokeText (String,Float,Float)
        | StrokeStyle String
        | TextAlign String
        | TextBaseline String
        | Transform (Float,Float,Float,Float,Float,Float)
        | Translate (Float,Float)

showJ :: Float -> String
showJ a = showFFloat (Just 3) a ""

showB :: Bool -> String
showB True = "true"
showB False = "false"

-- | size of the canvas
size :: Canvas (Float,Float)
size = Size

-- | read a specific event; wait for it if the event is not in queue.
readEvent :: EventName -> Canvas Event
readEvent nm = Get nm readEventQueue

-- | read a specific event; or return Nothing if the event is not in queue.
tryReadEvent :: EventName -> Canvas (Maybe Event)
tryReadEvent nm = Get nm tryReadEventQueue

