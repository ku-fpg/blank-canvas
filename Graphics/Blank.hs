{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank
        ( module Graphics.Blank
        , module Graphics.Blank.Events
        , module Graphics.Blank.Context
        , module Graphics.Blank.Canvas
        ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.IO.Class (liftIO)
import Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T

import Data.Aeson.TH (deriveJSON)
import Data.Aeson (Value, FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Vector as V

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Char
import Control.Monad

import Graphics.Blank.Events
import Graphics.Blank.Context
import Graphics.Blank.Canvas


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

