{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank
        ( module Graphics.Blank
        , module Graphics.Blank.Events
        ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Numeric

import Data.Aeson.TH (deriveJSON)
import Data.Aeson (Value, FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Vector as V

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Char
import Control.Monad

import Graphics.Blank.Events

data Context = Context
        { theSize :: (Float,Float)
        , theDraw :: MVar String
        , eventHandle :: MVar (Map EventName EventQueue)
        }


-- $(deriveJSON Prelude.id ''Event)

blankCanvas :: Int -> (Context -> IO ()) -> IO ()
blankCanvas port actions = do
   picture <- newEmptyMVar

   dims <- newEmptyMVar
   callbacks <- newMVar $ Map.empty

   -- perform the canvas writing actions
   -- in worker thread.
   forkIO $ do
       -- do not start until we know the screen size
       (w,h) <- takeMVar dims
       actions (Context (w,h) picture callbacks)


   scotty port $ do
        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ file "static/index.html"

        post "/start" $ do
            req <- jsonData
            liftIO $ tryPutMVar dims (req  :: (Float,Float))
            json ()

        post "/event" $ do
            NamedEvent nm event <- jsonData
            db <- liftIO $ readMVar callbacks
            liftIO $ print (nm,event)
            case Map.lookup nm db of
              Nothing -> json ()
              Just var -> do liftIO $ writeEventQueue var event -- perhaps use Chan?
                             json ()

        get "/canvas" $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            res <- liftIO $ tryTakeMVar picture
            case res of
              Just js -> do
                      text ("var c = getContext();" `T.append` T.pack js)
              Nothing -> do
                 -- hack, wait a second
                 liftIO $ threadDelay (1000 * 1000)
                 text (T.pack "redraw();")


-- | 'eventChan' gets the raw event channel for a specific event type.
eventChan :: Context -> EventName -> IO EventQueue
eventChan cxt@(Context _ canvas callbacks) a = do
        db <- takeMVar callbacks
        case Map.lookup a db of
          Just var -> do
            putMVar callbacks db
            return var
          Nothing -> do
            var <- newEventQueue
            putMVar callbacks $ Map.insert a var db
            sendToCanvas cxt (("register('" ++ map toLower (show a) ++ "');") ++)
            return var

-- Internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas (Context _ var _) cmds = putMVar var $ "var c = getContext(); " ++ cmds "redraw();"

send :: Context -> Canvas a -> IO a
send cxt@(Context (h,w) var callbacks) commands = send' commands id
  where
      send' :: Canvas a -> (String -> String) -> IO a

      send' (Bind (Return a) k)    cmds = send' (k a) cmds
      send' (Bind (Bind m k1) k2)  cmds = send' (Bind m (\ r -> Bind (k1 r) k2)) cmds
      send' (Bind (Command cmd) k) cmds = send' (k ()) (cmds . shows cmd)
      send' (Bind Size k)          cmds = send' (k (h,w)) cmds
      send' (Bind other k)         cmds = do
              res <- send' other cmds
              send' (k res) id

      send' (Get a op)             cmds = do
              -- clear the commands
              sendToCanvas cxt cmds
              -- get the channel for this event
              chan <- eventChan cxt a
              op chan

      send' (Return a)             cmds = do
              putMVar var $ "var c = getContext(); " ++ cmds "redraw();"
              return a
      send' other                  cmds = send' (Bind other Return) cmds

      sendCmds :: (String -> String) -> IO ()
      sendCmds cmds = putMVar var $ "var c = getContext(); " ++ cmds "redraw();"
{-
      send' (Get name)             cmds = do
              -- send the commands
              return undefined
-}
--      send' (Command draw) todo = todo ++ show commands
{-




-}
--    return ()


data StateOfInput = StateOfInput
        { mousePos :: Maybe (Int,Int)
        , mousePress :: Bool
        }


{-
   Browser
   Keyboard
   Mouse
     - down/up
   Touch
 -}

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
{-
  show (BeginPath)             = "c.beginPath();"
  show (ClosePath)             = "c.closePath();"
--  show (ClearRect a b c d)     = "c.clearRect(" ++ showJ a ++ "," ++ showJ b ++ "," ++ showJ c ++ "," ++ showJ d ++ ");"
  show (Fill)                  = "c.fill();"
  show (FillStyle any)         = "c.fillStyle = " ++ show any ++ ";"
  show (LineTo a b)            = "c.lineTo(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (LineWidth w)           = "c.lineWidth = " ++ showJ w ++ ";"
  show (MiterLimit f)          = "c.miterLimit = " ++ showJ f ++ ";"
  show (MoveTo a b)            = "c.moveTo(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (Restore)   	       = "c.restore();"
  show (Rotate f)   	       = "c.rotate(" ++ showJ f ++ ");"
  show (Scale a b)             = "c.scale(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (Save) 		       = "c.save();"
  show (Stroke)   	       = "c.stroke();"
  show (StrokeStyle any)       = "c.strokeStyle = " ++ show any ++ ";"
  show (Transform a b c d e f) = "c.transform(" ++ showJ a ++ "," ++ showJ b ++ "," ++ showJ c ++ "," ++ showJ d ++ "," ++ showJ e ++ "," ++ showJ f ++ ");"
  show (Translate a b)         = "c.translate(" ++ showJ a ++ "," ++ showJ b ++ ");"
-}
-- And our Deep DSL.

--keypress :: (Int -> IO ()) -> Command
--keypress f =

-----------------------------------------------------------------

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