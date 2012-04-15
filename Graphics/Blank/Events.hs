module Graphics.Blank.Events
        ( Event(..)
        , NamedEvent(..)
        , EventName(..)
        , EventQueue            -- abstract
        , writeEventQueue
        , readEventQueue
        , tryReadEventQueue
        , flushEventQueue
        , newEventQueue
        ) where

import Data.Aeson.TH (deriveJSON)
import Data.Aeson (Value, FromJSON(..))
import qualified Data.Aeson as A
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Char
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent


-- | Basic Event from Browser, the code is event type specific.
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
           (str,code,x,y) <- parseJSON o
           case Map.lookup str namedEventDB of
             Just n -> return $ NamedEvent n (Event code (Just (x,y)))
             Nothing -> do (str,code,(),()) <- parseJSON o
                           case Map.lookup str namedEventDB of
                             Just n -> return $ NamedEvent n (Event code Nothing)
                             Nothing -> fail "bad parse"

namedEventDB :: Map String EventName
namedEventDB = Map.fromList
                [ (map toLower (show n),n)
                | n <- [minBound..maxBound]
                ]

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

data EventQueue = EventQueue (MVar Event) (Chan Event)

writeEventQueue :: EventQueue -> Event -> IO ()
writeEventQueue (EventQueue _ chan) event = writeChan chan event

readEventQueue :: EventQueue -> IO Event
readEventQueue (EventQueue var _) = takeMVar var

tryReadEventQueue :: EventQueue -> IO (Maybe Event)
tryReadEventQueue (EventQueue var _) = tryTakeMVar var

flushEventQueue :: EventQueue -> IO ()
flushEventQueue q = do
        r <- tryReadEventQueue q
        case r of
          Just {} -> flushEventQueue q
          Nothing -> return ()

newEventQueue :: IO EventQueue
newEventQueue = do
        var <- newEmptyMVar
        chan <- newChan
        forkIO $ forever $ do
            event <- readChan chan
            putMVar var event
        return $ EventQueue var chan
