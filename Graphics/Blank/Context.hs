module Graphics.Blank.Context where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char

import qualified Web.KansasComet as KC

import Graphics.Blank.Events

-- | 'Context' is our abstact handle into a specific 2d-context inside a browser.
data Context = Context
        { theComet    :: KC.Document                -- ^ The mechansims for sending commands
        , eventQueue  :: EventQueue                 -- ^ A single (typed) event queue
        }


-- | internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas cxt cmds = do
        KC.send (theComet cxt) $ "{var c = getContext();" ++ cmds "}"

-- | wait for any event
wait :: Context -> IO NamedEvent
wait c = atomically $ readTChan (eventQueue c)
