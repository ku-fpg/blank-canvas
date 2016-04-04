{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Blank.Events where

import Control.Applicative
import Control.Concurrent.STM

import Data.Aeson (FromJSON(..), Value(..), ToJSON(..))
import Data.Aeson.Types ((.:), (.=), object)
import Data.Text.Lazy (Text)

import TextShow.TH (deriveTextShow)

-- | 'EventName' mirrors event names from jQuery, and uses lowercase.
-- Possible named events
-- 
-- * @keypress@, @keydown@, @keyup@
-- * @mouseDown@, @mouseenter@, @mousemove@, @mouseout@, @mouseover@, @mouseup@
type EventName = Text

-- | 'EventQueue' is an STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan Event

-- | Basic event from browser. See <http://api.jquery.com/category/events/> for details.
data Event = Event
        { eMetaKey :: Bool
        , ePageXY  :: Maybe (Double, Double)
        , eType    :: EventName          -- "Describes the nature of the event." jquery
        , eWhich   :: Maybe Int          -- magic code for key presses
        }
        deriving (Eq, Ord, Show)
$(deriveTextShow ''Event)

instance FromJSON Event where
   parseJSON (Object v) = Event <$> ((v .: "metaKey")              <|> return False)
                                <*> (Just <$> (v .: "pageXY")      <|> return Nothing)
                                <*> (v .: "type")
                                <*> (Just <$> (v .: "which")       <|> return Nothing)
   parseJSON _ = fail "no parse of Event"    

instance ToJSON Event where
   toJSON e = object 
            $ ((:) ("metaKey" .=  eMetaKey e))
            $ (case ePageXY e of
                 Nothing -> id
                 Just (x,y) -> (:) ("pageXY" .= (x,y)))
            $ ((:) ("type" .= eType e))
            $ (case eWhich e of
                 Nothing -> id
                 Just w -> (:) ("which" .= w))
            $ []
