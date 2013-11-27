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
        , eventQueue  :: EventQueue -- now a single event queueMVar (Map EventName EventQueue)
        , sessionNo   :: Int
        }

-- 'events' gets a copy of the events Queue
{-# DEPRECATED events "use readEvent(s) or tryReadEvent(s)" #-}
events :: Context -> IO EventQueue
events = return . eventQueue

-- | 'register' makes sure the named events are registered.
register :: Context -> [EventName] -> IO ()
register cxt@(Context _ _ regs _ num) nms = do
        db <- takeMVar regs
        let new = Set.difference (Set.fromList nms) db
        sequence_ [ sendToCanvas cxt (("register('" ++ map toLower (show nm) ++ "'," ++ show num ++ ");") ++)
                  | nm <- Set.toList new
                  ]
        if Set.null new
        then putMVar regs $ db
        else putMVar regs $ (db `Set.union` new)

-- | internal command to send a message to the canvas.
sendToCanvas :: Context -> ShowS -> IO ()
sendToCanvas (Context _ var _ _ num) cmds = putMVar var $ "if (session == " ++ show num ++ "){var c = getContext();" ++ cmds "}"
