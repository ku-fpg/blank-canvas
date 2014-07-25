{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Graphics.Blank.Events where

import Control.Applicative ((<|>), (<$>), (<*>))
import Control.Concurrent.STM

import Data.Aeson (FromJSON(..), Value(..), ToJSON(..))
import Data.Aeson.Types ((.:), (.=), object)
import Data.Text (Text)

-- | Basic Event from Browser; see <http://api.jquery.com/category/events/event-object/> for details.
data Event = Event
        { eMetaKey :: Bool
        , ePageXY  :: Maybe (Float,Float)
        , eType    :: EventName          -- "Describes the nature of the event." jquery
        , eWhich   :: Maybe Int          -- magic code for key presses
        }
        deriving (Show)


instance FromJSON Event where
   parseJSON (Object v) = Event <$> ((v .: "eMetaKey")              <|> return False)
                                <*> (Just <$> (v .: "ePageXY")      <|> return Nothing)
                                <*> (v .: "eType")
                                <*> (Just <$> (v .: "eWhich")       <|> return Nothing)
   parseJSON _ = fail "no parse of Event"    

instance ToJSON Event where
   toJSON e = object 
            $ ((:) ("eMetaKey" .=  eMetaKey e))
            $ (case ePageXY e of
                 Nothing -> id
                 Just (x,y) -> (:) ("ePageXY" .= (x,y)))
            $ ((:) ("eType" .= eType e))
            $ (case eWhich e of
                 Nothing -> id
                 Just w -> (:) ("eWhich" .= w))
            $ []

-- | 'EventName' mirrors event names from jquery, and use lower case.
--   Possible named events
--
--     * keypress, keydown, keyup
--     * mouseDown, mouseenter, mousemove, mouseout, mouseover, mouseup
-- 
type EventName = Text

-- | EventQueue is a STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan Event

