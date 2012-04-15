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

showJ a = showFFloat (Just 3) a ""

instance Show Command where
  show BeginPath = "c.beginPath();"
  show (ClearRect (a1,a2,a3,a4)) = "c.clearRect(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ ");"
  show ClosePath = "c.closePath();"
  show Fill = "c.fill();"
  show (FillStyle (a1)) = "c.fillStyle = (" ++ show a1 ++ ");"
  show (LineTo (a1,a2)) = "c.lineTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show (LineWidth (a1)) = "c.lineWidth = (" ++ showJ a1 ++ ");"
  show (MiterLimit (a1)) = "c.miterLimit = (" ++ showJ a1 ++ ");"
  show (MoveTo (a1,a2)) = "c.moveTo(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Restore = "c.restore();"
  show (Rotate (a1)) = "c.rotate(" ++ showJ a1 ++ ");"
  show (Scale (a1,a2)) = "c.scale(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"
  show Save = "c.save();"
  show Stroke = "c.stroke();"
  show (StrokeStyle (a1)) = "c.strokeStyle = (" ++ show a1 ++ ");"
  show (Transform (a1,a2,a3,a4,a5,a6)) = "c.transform(" ++ showJ a1 ++ "," ++ showJ a2 ++ "," ++ showJ a3 ++ "," ++ showJ a4 ++ "," ++ showJ a5 ++ "," ++ showJ a6 ++ ");"
  show (Translate (a1,a2)) = "c.translate(" ++ showJ a1 ++ "," ++ showJ a2 ++ ");"

----------------------------------------------------------------

-- | size of the canvas
size :: Canvas (Float,Float)
size = Size

readEvent :: EventName -> Canvas Event
readEvent nm = Get nm readEventQueue

tryReadEvent :: EventName -> Canvas (Maybe Event)
tryReadEvent nm = Get nm tryReadEventQueue

flushEvents :: EventName -> Canvas ()
flushEvents nm = Get nm flushEventQueue

-----------------------------------------------------------------

beginPath :: Canvas ()
beginPath = Command BeginPath

clearRect :: (Float,Float,Float,Float) -> Canvas ()
clearRect = Command . ClearRect

closePath :: Canvas ()
closePath = Command ClosePath

fill :: Canvas ()
fill = Command Fill

fillStyle :: String -> Canvas ()
fillStyle = Command . FillStyle

lineTo :: (Float,Float) -> Canvas ()
lineTo = Command . LineTo

lineWidth :: Float -> Canvas ()
lineWidth = Command . LineWidth

miterLimit :: Float -> Canvas ()
miterLimit = Command . MiterLimit

moveTo :: (Float,Float) -> Canvas ()
moveTo = Command . MoveTo

restore :: Canvas ()
restore = Command Restore

rotate :: Float -> Canvas ()
rotate = Command . Rotate

scale :: (Float,Float) -> Canvas ()
scale = Command . Scale

save :: Canvas ()
save = Command Save

stroke :: Canvas ()
stroke = Command Stroke

strokeStyle :: String -> Canvas ()
strokeStyle = Command . StrokeStyle

transform :: (Float,Float,Float,Float,Float,Float) -> Canvas ()
transform = Command . Transform

translate :: (Float,Float) -> Canvas ()
translate = Command . Translate
