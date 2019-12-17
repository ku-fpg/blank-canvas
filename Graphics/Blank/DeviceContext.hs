{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.DeviceContext where

import           Control.Concurrent.STM

import           Data.Set (Set)
import           Data.Text.Lazy (Text, toStrict)

import           Graphics.Blank.Events
import           Graphics.Blank.JavaScript

import           Graphics.Blank.Instr

import           Prelude.Compat

-- import           TextShow (Builder, toText)

--import qualified Web.Scotty.Comet as KC
import Network.JavaScript as JS

-- | 'DeviceContext' is the abstract handle into a specific 2D context inside a browser.
-- Note that the JavaScript API concepts of
-- @<https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D CanvasRenderingContext2D>@ and
-- @<https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement HTMLCanvasElement>@
-- are conflated in @blank-canvas@. Therefore, there is no
-- @<https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement/getContext getContext()>@ method;
-- rather, @getContext()@ is implied (when using 'send').
data DeviceContext = DeviceContext
        { theComet             :: JS.Engine       -- ^ The mechanisms for sending commands
        , eventQueue           :: EventQueue      -- ^ A single (typed) event queue
        , ctx_width            :: !Int
        , ctx_height           :: !Int
        , ctx_devicePixelRatio :: !Double
        , localFiles           :: TVar (Set Text) -- ^ approved local files
        , weakRemoteMonad      :: Bool            -- ^ use a weak remote monad for debugging
        }

instance Image DeviceContext where
  jsImage = jsImage . deviceCanvasContext
  width  = fromIntegral . ctx_width
  height = fromIntegral . ctx_height

deviceCanvasContext :: DeviceContext -> CanvasContext
deviceCanvasContext cxt = CanvasContext Nothing (ctx_width cxt) (ctx_height cxt)


-- | 'devicePixelRatio' returns the device's pixel ratio as used. Typically, the
-- browser ignores @devicePixelRatio@ in the canvas, which can make fine details
-- and text look fuzzy. Using the query @?hd@ on the URL, @blank-canvas@ attempts
-- to use the native @devicePixelRatio@, and if successful, 'devicePixelRatio' will
-- return a number other than 1. You can think of 'devicePixelRatio' as the line
-- width to use to make lines look one pixel wide.
devicePixelRatio ::  DeviceContext -> Double
devicePixelRatio = ctx_devicePixelRatio

-- | Internal command to send a message to the canvas.
sendToCanvas :: DeviceContext -> Instr -> IO ()
sendToCanvas cxt cmds = do
    print "sendToCanvas"	    
--    KC.send (theComet cxt) . toStrict . toLazyText $ surround "syncToFrame(function(){"  "});" <> cmds

-- | Wait for any event. Blocks.
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
