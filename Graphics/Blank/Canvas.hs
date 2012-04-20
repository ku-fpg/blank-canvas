{-# LANGUAGE TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank.Canvas where

import Graphics.Blank.Events
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

-- HTML5 Canvas assignments: FillStyle, Font, LineCap, LineJoin, LineWidth, MiterLimit, StrokeStyle, TextAlign, TextBaseline
data Command
        -- regular HTML5 canvas commands
        = Arc (Float,Float,Float,Float,Float,Bool)
        | BeginPath
        | BezierCurveTo (Float,Float,Float,Float,Float,Float)
        | ClearRect (Float,Float,Float,Float)
        | ClosePath
        | Fill
        | FillStyle String
        | FillText (String,Float,Float)
        | Font String
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

