{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.Context where

import Control.Concurrent.STM
import Control.Monad

import qualified Web.Scotty.Comet as KC

import Graphics.Blank.Events
import Data.Monoid((<>))
import qualified Data.Text as T

-- | 'Context' is our abstact handle into a specific 2d-context inside a browser.
-- Note that the JavaScript API concepts of 2D-Context and Canvas
-- are conflated in blank-canvas. Therefore, there is no 'getContext' method,
-- rather 'getContext' is implied (when using 'send').

data Context = Context
        { theComet    :: KC.Document                -- ^ The mechansims for sending commands
        , eventQueue  :: EventQueue                 -- ^ A single (typed) event queue
        }


-- | internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas cxt cmds = do
        KC.send (theComet cxt) $ "try{" <> T.pack (cmds "}catch(e){alert('JavaScript Failure: '+e.message);}")

-- | wait for any event
wait :: Context -> IO Event
wait c = atomically $ readTChan (eventQueue c)

-- | get the next event if it exists
tryGet :: Context -> IO (Maybe Event)
tryGet cxt = atomically $ do
    b <- isEmptyTChan (eventQueue cxt)
    if b 
    then return Nothing
    else liftM Just $ readTChan (eventQueue cxt)

-- | 'flush' all the current events, returning them all to the user.
flush :: Context -> IO [Event]
flush cxt = atomically $ loop
  where loop = do 
          b <- isEmptyTChan (eventQueue cxt)
          if b then return [] else do
                 e <- readTChan (eventQueue cxt)
                 es <- loop
                 return (e : es)
