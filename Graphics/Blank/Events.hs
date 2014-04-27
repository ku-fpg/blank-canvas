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
        , writeEventQueue
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
           case Map.lookup str namedEventDB of
             Just n -> fmap (NamedEvent n) (opt1 <|> opt2)
             Nothing -> fail "bad parse"
    where
           opt1 = do (_::String,code,x,y) <- parseJSON o
                     return $ Event code (Just (x,y))
           opt2 = do (_::String,code,_::Value,_::Value) <- parseJSON o
                     return $ Event code Nothing


namedEventDB :: Map String EventName
namedEventDB = Map.fromList
                [ (map toLower (show n),n)
                | n <- [minBound..maxBound]
                ]

-- | 'EventName' mirrors event names from jquery, where 'map toLower (show name)' gives
-- the jquery event name.
data EventName
        -- Keys
        = KeyPress
        | KeyDown
        | KeyUp
        -- Mouse
        | MouseDown
        | MouseEnter
        | MouseMove
        | MouseOut
        | MouseOver
        | MouseUp
        deriving (Eq, Ord, Show, Enum, Bounded)

-- | EventQueue is a STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan NamedEvent

writeEventQueue :: EventQueue -> NamedEvent -> IO ()
writeEventQueue q e = atomically $ writeTChan q e

-- | wait for a specific, named event.
wait :: EventQueue -> EventName -> IO Event
wait q nm = atomically $ do
   NamedEvent nm' e <- readTChan q
   check (nm == nm')
   return e

