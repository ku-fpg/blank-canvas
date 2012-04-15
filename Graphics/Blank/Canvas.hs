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

-- HTML5 Canvas assignments: FillStyle, LineWidth, MiterLimit, StrokeStyle
data Command
        -- regular HTML5 canvas commands
        = BeginPath
        | ClearRect (Float,Float,Float,Float)
        | ClosePath
        | Fill
        | FillStyle String
        | LineCap String
        | LineTo (Float,Float)
        | LineWidth Float
        | MiterLimit Float
        | MoveTo (Float,Float)
        | Restore
        | Rotate Float
        | Scale (Float,Float)
        | Save
        | Stroke
        | StrokeStyle String
        | Transform (Float,Float,Float,Float,Float,Float)
        | Translate (Float,Float)

rgba :: (Int,Int,Int,Int) -> String
rgba (r,g,b,a) = "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ ")"

showJ :: Float -> String
showJ a = showFFloat (Just 3) a ""

-- | size of the canvas
size :: Canvas (Float,Float)
size = Size

readEvent :: EventName -> Canvas Event
readEvent nm = Get nm readEventQueue

tryReadEvent :: EventName -> Canvas (Maybe Event)
tryReadEvent nm = Get nm tryReadEventQueue

flushEvents :: EventName -> Canvas ()
flushEvents nm = Get nm flushEventQueue
