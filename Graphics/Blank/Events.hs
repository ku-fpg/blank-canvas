{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Blank.Events
        ( -- * Events
          Event(..)
        , NamedEvent(..)
        , EventName(..)
         -- * Event Queue
        , EventQueue            -- not abstract
        , wait
        -- * Internal
--        , writeEventQueue
        ) where

import Data.Aeson (FromJSON(..), Value)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char
import Control.Applicative((<|>))
import Control.Concurrent.STM

-- | Basic Event from Browser, the code is event-type specific.
data Event = Event
        { jsCode  :: Int
        , jsMouse :: Maybe (Int,Int)
        }
        deriving (Show)

-- | When an event is sent to the application, it always has a name.
data NamedEvent = NamedEvent EventName Event
        deriving (Show)

instance FromJSON NamedEvent where
   parseJSON o = do
           (str::String,_::Value,_::Value,_::Value) <- parseJSON o
           fmap (NamedEvent str) (opt1 <|> opt2)
    where
           opt1 = do (_::String,code,x,y) <- parseJSON o
                     return $ Event code (Just (x,y))
           opt2 = do (_::String,code,_::Value,_::Value) <- parseJSON o
                     return $ Event code Nothing

-- | 'EventName' mirrors event names from jquery, and use lower case.
--   Possible named events
--    * keypress, keydown, keyup
--    * mouseDown, mouseenter, mousemove, mouseout, mouseover, mouseup
type EventName = String

-- | EventQueue is a STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan NamedEvent

-- | wait for a specific, named event.
wait :: EventQueue -> EventName -> IO Event
wait q nm = atomically $ do
   NamedEvent nm' e <- readTChan q
   check (nm == nm')
   return e

{-
    DEPRECATED EventQueue, readEventQueue, tryReadEventQueue "use readEvent(s) or tryReadEvent(s)" 
readEventQueue :: EventQueue -> IO NamedEvent
readEventQueue q = atomically $ readTChan q


tryReadEventQueue :: EventQueue -> IO (Maybe NamedEvent)
tryReadEventQueue q = atomically $ do
        b <- isEmptyTChan q
        if b then return Nothing
             else liftM Just (readTChan q)

newEventQueue :: IO EventQueue
newEventQueue = atomically newTChan

-}