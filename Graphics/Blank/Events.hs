{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Graphics.Blank.Events where

import Data.Aeson (FromJSON(..), Value(..), ToJSON(..))
import Data.Aeson.Types (Parser, (.:), (.=), object)
import Control.Applicative((<|>),(<$>),(<*>))
import Control.Concurrent.STM

-- | Basic Event from Browser; see http://api.jquery.com/category/events/event-object/ for details.
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

        {-
   parseJSON o = do
           (str::String,_::Value,_::Value,_::Value) <- parseJSON o
           fmap (NamedEvent str) (opt1 <|> opt2)
    where
           opt1 = do (_::String,code,x,y) <- parseJSON o
                     return $ Event code (Just (x,y))
           opt2 = do (_::String,code,_::Value,_::Value) <- parseJSON o
                     return $ Event code Nothing
-}
-- | 'EventName' mirrors event names from jquery, and use lower case.
--   Possible named events
--    * keypress, keydown, keyup
--    * mouseDown, mouseenter, mousemove, mouseout, mouseover, mouseup
type EventName = String

-- | EventQueue is a STM channel ('TChan') of 'Event's.
-- Intentionally, 'EventQueue' is not abstract.
type EventQueue = TChan Event

