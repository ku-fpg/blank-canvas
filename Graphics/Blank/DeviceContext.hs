{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.DeviceContext where

import           Control.Concurrent.STM

import           Data.Set (Set)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           Data.Text.Lazy hiding (Text)
import           Data.Text.Lazy.Builder

import           Graphics.Blank.Events
import           Graphics.Blank.JavaScript

import qualified Web.Scotty.Comet as KC

-- | 'Context' is our abstact handle into a specific 2d-context inside a browser.
-- Note that the JavaScript API concepts of 2D-Context and Canvas
-- are conflated in blank-canvas. Therefore, there is no 'getContext' method,
-- rather 'getContext' is implied (when using 'send').

data DeviceContext = DeviceContext
        { theComet             :: KC.Document     -- ^ The mechansims for sending commands
        , eventQueue           :: EventQueue      -- ^ A single (typed) event queue
        , ctx_width            :: !Int
        , ctx_height           :: !Int
        , ctx_devicePixelRatio :: !Double
        , localFiles           :: TVar (Set Text) -- ^ approved local files
        }

instance Image DeviceContext where
  jsImage = jsImage . deviceCanvasContext
  width  = fromIntegral . ctx_width
  height = fromIntegral . ctx_height

deviceCanvasContext :: DeviceContext -> CanvasContext
deviceCanvasContext cxt = CanvasContext 0 (ctx_width cxt) (ctx_height cxt)

-- ** 'devicePixelRatio' returns the Device Pixel Ratio as used. Typically, the browser ignore devicePixelRatio in the canvas,
--   which can make fine details and text look fuzzy. Using the query "?hd" on the URL, blank-canvas attempts
--   to use the native devicePixelRatio, and if successful, 'devicePixelRatio' will return a number other than 1.
--   You can think of devicePixelRatio as the line width to use to make lines look one pixel wide.

devicePixelRatio ::  DeviceContext -> Double
devicePixelRatio = ctx_devicePixelRatio

-- | internal command to send a message to the canvas.
sendToCanvas :: DeviceContext -> (Builder -> Builder) -> IO ()
sendToCanvas cxt cmds = do
        KC.send (theComet cxt) . toStrict . toLazyText $ "try{" <> cmds "}catch(e){alert('JavaScript Failure: '+e.message);}"

-- | wait for any event. blocks.
wait :: DeviceContext -> IO Event
wait c = atomically $ readTChan (eventQueue c)

-- | 'flush' all the current events, returning them all to the user. Never blocks.
flush :: DeviceContext -> IO [Event]
flush cxt = atomically $ loop
  where loop = do
          b <- isEmptyTChan (eventQueue cxt)
          if b then return [] else do
                 e <- readTChan (eventQueue cxt)
                 es <- loop
                 return (e : es)
