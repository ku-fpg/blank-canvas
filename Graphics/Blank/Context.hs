module Graphics.Blank.Context where

import Control.Concurrent
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Char

import Graphics.Blank.Events

-- | 'Context' is our abstact handle into a specific 2d-context inside a browser.
data Context = Context
        { theSize     :: (Float,Float)
        , theDraw     :: MVar String
        , eventRegs   :: MVar (Set EventName)       -- events that are registered
        , eventQueue  :: EventQueue -- now a single event queue
        , sessionNo   :: Int
        }

-- 'events' returns the events Queue
events :: Context -> EventQueue
events = eventQueue

-- | 'register' makes sure the named event is registered.
register :: Context -> EventName -> IO ()
register cxt@(Context _ _ regs _ num) newEvent = do
        db <- takeMVar regs
        let new = Set.difference (Set.fromList [newEvent]) db
        sequence_ [ sendToCanvas cxt (("register('" ++ map toLower (show nm) ++ "'," ++ show num ++ ");") ++)
                  | nm <- Set.toList new
                  ]
        if Set.null new
        then putMVar regs $ db
        else putMVar regs $ (db `Set.union` new)

-- | internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas (Context _ var _ _ num) cmds = putMVar var $ "if (session == " ++ show num ++ "){var c = getContext();" ++ cmds "}"
