module Graphics.Blank.Context where

import Control.Concurrent
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char

import Graphics.Blank.Events

-- | 'Context' is our abstact handle into a specific 2d-context inside a browser.
data Context = Context
        { theSize     :: (Float,Float)
        , theDraw     :: MVar String
        , eventHandle :: MVar (Map EventName EventQueue)
        , sessionNo   :: Int
        }

-- | 'events' gets the raw event queue for a specific event type.
events :: Context -> EventName -> IO EventQueue
events cxt@(Context _ _ callbacks num) a = do
        db <- takeMVar callbacks
        case Map.lookup a db of
          Just var -> do
            putMVar callbacks db
            return var
          Nothing -> do
            var <- newEventQueue
            putMVar callbacks $ Map.insert a var db
            sendToCanvas cxt (("register('" ++ map toLower (show a) ++ "'," ++ show num ++ ");") ++)
            return var

-- | internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas (Context _ var _ num) cmds = putMVar var $ "if (session == " ++ show num ++ "){var c = getContext();" ++ cmds "}"
