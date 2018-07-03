{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-|
Module:      Graphics.Blank
Copyright:   (C) 2014-2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Beta
Portability: GHC

@blank-canvas@ is a Haskell binding to the complete
<https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API HTML5 Canvas API>.
@blank-canvas@ allows Haskell users to write, in Haskell,
interactive images onto their web browsers. @blank-canvas@ gives
the users a single full-window canvas, and provides many
well-documented functions for rendering images.
-}
module Graphics.Blank
        (
         -- * Starting @blank-canvas@
          blankCanvas
        , Options(..)
        , BundlingStrategy(..)
          -- ** 'send'ing to the Graphics 'DeviceContext'
        , DeviceContext       -- abstact
        , send
        , sendW
        , sendS
        , sendA
          -- * HTML5 Canvas API
          -- | See <https://developer.mozilla.org/en-US/docs/Web/API/Canvas_API> for the JavaScript
          --   version of this API.
        , Canvas        -- abstract
          -- ** Canvas element
        , height
        , width
        , toDataURL
          -- ** 2D Context
        , save
        , restore
          -- ** Transformation
        , scale
        , rotate
        , translate
        , transform
        , setTransform
          -- ** Image drawing
        , Image -- abstract class
        , drawImage
          -- ** Compositing
        , globalAlpha
        , globalCompositeOperation
          -- ** Line styles
        , lineWidth
        , lineCap
        , lineJoin
        , miterLimit
        , LineEndCap(..)
        , butt
        , square
        , LineJoinCorner(..)
        , bevel
        , miter
          -- ** Colors, styles and shadows
        , strokeStyle
        , fillStyle
        , shadowOffsetX
        , shadowOffsetY
        , shadowBlur
        , shadowColor
        , createLinearGradient
        , createRadialGradient
        , createPattern
        , addColorStop
        , RepeatDirection(..)
        , repeat_
        , repeatX
        , repeatY
        , noRepeat
        , CanvasGradient
        , CanvasPattern
          -- ** Paths
        , beginPath
        , closePath
        , fill
        , stroke
        , clip
        , moveTo
        , lineTo
        , quadraticCurveTo
        , bezierCurveTo
        , arcTo
        , arc
        , rect
        , isPointInPath
          -- ** Text
        , font
        , textAlign
        , textBaseline
        , fillText
        , strokeText
        , measureText
        , TextAnchorAlignment(..)
        , start
        , end
        , center
        , left
        , right
        , TextBaselineAlignment(..)
        , top
        , hanging
        , middle
        , alphabetic
        , ideographic
        , bottom
        , TextMetrics(..)
          -- ** Rectangles
        , clearRect
        , fillRect
        , strokeRect
          -- ** Pixel manipulation
        , getImageData
        , putImageData
        , ImageData(..)
          -- * Type information
        , Alpha
        , Degrees
        , Interval
        , Percentage
        , Radians
        , RoundProperty(..)
        -- * @blank-canvas@ Extensions
        -- ** Reading from 'Canvas'
        , newImage
        , CanvasImage -- abstract
          -- ** Audio functionality
        , currentTimeAudio
        , durationAudio -- subject to change
        , indexAudio
        , playAudio
        , pauseAudio
        , setCurrentTimeAudio
        , setLoopAudio
        , setMutedAudio
        , setPlaybackRateAudio
        , setVolumeAudio
        , newAudio
        , CanvasAudio
         -- ** 'DeviceContext' attributes
        , devicePixelRatio
         -- ** 'CanvasContext', and off-screen Canvas.
        , CanvasContext
        , newCanvas
        , with
        , myCanvasContext
        , deviceCanvasContext
         -- ** Syncing
        , sync
         -- ** Debugging
        , console_log
        , eval
        , JSArg(..)
        , PacketProfile(..)
        , readPacketProfile
         -- ** Drawing Utilities
        , module Graphics.Blank.Utils
         -- ** Events
        , trigger
        , eventQueue
        , wait
        , flush
        , frame
        , Event(..)
        , EventName
        , EventQueue
        -- ** Cursor manipulation
        , cursor
        -- ** Middleware
        , local_only
        ) where

import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class

import           Data.Aeson                           (Result (..), fromJSON)
import           Data.List                            as L
import qualified Data.Map                             as M (empty, lookup)
import qualified Data.Set                             as S
import qualified Data.Text                            as ST
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Text.Lazy                       (Text)
import qualified Data.Text.Lazy                       as T
import qualified Data.Text.Lazy                       as LT

import           Graphics.Blank.Canvas                hiding (addColorStop,
                                                       cursor)
import qualified Graphics.Blank.Canvas                as Canvas
import           Graphics.Blank.DeviceContext         hiding (profiling)
import           Graphics.Blank.DeviceContext         (Packetize (packetize))
import           Graphics.Blank.Events
import           Graphics.Blank.Generated             hiding (fillStyle, font,
                                                       shadowColor, strokeStyle)
import qualified Graphics.Blank.Generated             as Generated
import           Graphics.Blank.JavaScript            hiding (durationAudio,
                                                       height, indexAudio,
                                                       width)
import qualified Graphics.Blank.JavaScript            as JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Utils


import qualified Network.HTTP.Types                   as H
import qualified Network.JavaScript                   as JSB
import           Network.Mime                         (defaultMimeMap,
                                                       fileNameExtensions)
import           Network.Wai                          (Middleware, responseLBS)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Local         as Local
import           Network.Wai.Middleware.RequestLogger

import           Paths_blank_canvas

import           Control.Category
import           Prelude.Compat                       hiding (id, (.))

import           Web.Scotty                           (file, get, scottyApp)
import qualified Web.Scotty                           as Scotty
import qualified Web.Scotty.Comet                     as KC

import           Control.Natural
import qualified Control.Natural                      as N
import           Control.Remote.Monad
import qualified Control.Remote.Packet.Applicative    as AP
import qualified Control.Remote.Packet.Strong         as SP
import qualified Control.Remote.Packet.Weak           as WP




-- | 'blankCanvas' is the main entry point into @blank-canvas@.
-- A typical invocation would be
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >module Main where
-- >
-- >import Graphics.Blank
-- >
-- >main = blankCanvas 3000 $ \ context -> do
-- >        send context $ do
-- >                moveTo(50,50)
-- >                lineTo(200,100)
-- >                lineWidth 10
-- >                strokeStyle "red"
-- >                stroke()
-- >

blankCanvas :: Options -> (DeviceContext -> IO ()) -> IO ()
blankCanvas opts actions = do
   dataDir <- getDataDir

   kComet <- KC.kCometPlugin

   locals :: TVar (S.Set Text) <- atomically $ newTVar S.empty

   app <- scottyApp $ do
        Scotty.middleware logStdoutDev
        Scotty.middleware $ JSB.start $ \ engine -> do
          queue <- liftIO $ atomically newTChan
          prof <- if profiling opts
                  then atomically (Just <$> newTVar M.empty)
                  else return Nothing
          JSB.addListener engine $ \ val -> case fromJSON val of
            Success (event :: Event) -> atomically $ writeTChan queue event
            _                        -> return ()
          let bootstrapContext = CanvasContext 0 300 300
          sequence_ [ JSB.send engine $ packetize $ Command (Register nm) bootstrapContext
                    | nm <- events opts ]
          DeviceAttributes w h dpr <- JSB.send engine $ packetize $ Query Device bootstrapContext
          let cxt = DeviceContext engine queue w h dpr locals (bundling opts) prof

          actions cxt `catch` \ (e :: SomeException) -> do
               print ("Exception in blank-canvas application:" :: String)
               print e
               throw e

        sequence_ [ Scotty.middleware ware
                  | ware <- middleware opts
                  ]

        get "/"                 $ file $ dataDir ++ "/static/index.html"
        get "/jquery.js"        $ file $ dataDir ++ "/static/jquery.js"
        get "/jquery-json.js"   $ file $ dataDir ++ "/static/jquery-json.js"
        get "/kansas-comet.js"  $ file  kComet

        -- There has to be a better way of doing this, using function, perhaps?
        get (Scotty.regex "^/(.*)$") $ do
          fileName :: Text <- Scotty.param "1"
          db <- liftIO $ atomically $ readTVar locals
          if fileName `S.member` db || True
          then do
            let mime = mimeType fileName
            Scotty.setHeader "Content-Type" mime
            file (root opts ++ "/" ++ T.unpack fileName)
          else
            Scotty.next

        return ()

   runSettings (setPort (port opts)
               $ setTimeout 5
                 defaultSettings
               ) app

generalSend :: forall m a . RunMonad m
            => (DeviceContext -> m Prim :~> IO) -> DeviceContext -> Canvas a -> IO a
generalSend f cxt c = do
    -- XXX: Is it ok to hardcode 0 as the start value here?
    -- AJG: No, its not.
   let m0 :: RemoteMonad Prim (a,Int)
       m0 = runCanvas (deviceCanvasContext cxt) 0 c
   (a,_) <- runMonad (f cxt) N.# m0
   return a

sendA :: DeviceContext -> Canvas a -> IO a
sendA = generalSend (\cxt -> wrapNT (sendA' cxt))

sendS :: DeviceContext -> Canvas a -> IO a
sendS = generalSend (\cxt -> wrapNT (sendS' cxt))

sendW :: DeviceContext -> Canvas a -> IO a
sendW = generalSend (\cxt -> wrapNT (sendW' cxt))


sendA' :: DeviceContext -> AP.ApplicativePacket Prim a -> IO a
sendA' = sendToCanvas

sendS' :: DeviceContext -> SP.StrongPacket Prim a -> IO a
sendS' = sendToCanvas

sendW' :: DeviceContext -> WP.WeakPacket Prim a -> IO a
sendW' = sendToCanvas

-- | Sends a set of canvas commands to the 'Canvas'. Attempts
-- to common up as many commands as possible. Should not crash.
send :: DeviceContext -> Canvas a -> IO a
send dc c = case remoteBundling dc of
            Weak   -> sendW dc c
            Strong -> sendS dc c
            Appl   -> sendA dc c

local_only :: Middleware
local_only = Local.local $ responseLBS H.status403 [("Content-Type", "text/plain")] "local access only"

mimeType :: Text -> Text
mimeType filePath = LT.fromStrict $ go $ fileNameExtensions $ LT.toStrict filePath
  where
    go [] = error $ "do not understand mime type for : " ++ show filePath
    go (e:es) = case M.lookup e defaultMimeMap of
                     Nothing -> go es
                     Just mt -> decodeUtf8 mt

-------------------------------------------------

-- | Additional @blank-canvas@ settings. The defaults can be used by creating
-- 'Options' as a 'Num'. For example, @'blankCanvas' 3000@ uses the default 'Options'
-- on port 3000.
data Options = Options
        { port       :: Int              -- ^ On which port do we issue @blank-canvas@?
        , events     :: [EventName]      -- ^ To which events does the canvas listen? Default: @[]@
        , debug      :: Bool             -- ^ Turn on debugging. Default: @False@
        , root       :: String           -- ^ Location of the static files. Default: @\".\"@
        , middleware :: [Middleware] -- ^ Extra middleware(s) to be executed. Default: @['local_only']@
        , bundling   :: BundlingStrategy -- ^ select remote monad bundling (default Appl)
        , profiling  :: Bool         -- ^ turn on profiling of packets. Default: @False@
        }

instance Num Options where
    (+) = error "no arithmetic for Blank Canvas Options"
    (-) = error "no arithmetic for Blank Canvas Options"
    (*) = error "no arithmetic for Blank Canvas Options"
    abs = error "no arithmetic for Blank Canvas Options"
    signum = error "no arithmetic for Blank Canvas Options"
    fromInteger n = Options { port = fromInteger n
                            , events = []
                            , debug = False
                            , root = "."
                            , middleware = [local_only]
                            , bundling = Appl
                            , profiling = False
                            }

-------------------------------------------------
-- These are monomorphic versions of functions defined to curb type ambiguity errors.

-- | Sets the color used to fill a drawing (@\"black\"@ by default).
--
-- ==== __Examples__
--
-- @
-- 'fillStyle' \"red\"
-- 'fillStyle' \"#00FF00\"
-- @
fillStyle :: ST.Text -> Canvas ()
fillStyle = Generated.fillStyle . LT.fromStrict

-- | Sets the text context's font properties.
--
-- ==== __Examples__
--
-- @
-- 'font' \"40pt \'Gill Sans Extrabold\'\"
-- 'font' \"80% sans-serif\"
-- 'font' \"bold italic large serif\"
-- @
font :: ST.Text -> Canvas ()
font = Generated.font . LT.fromStrict

-- | Sets the color used for strokes (@\"black\"@ by default).
--
-- ==== __Examples__
--
-- @
-- 'strokeStyle' \"red\"
-- 'strokeStyle' \"#00FF00\"
-- @
strokeStyle :: ST.Text -> Canvas ()
strokeStyle = Generated.strokeStyle . LT.fromStrict

-- | Sets the color used for shadows.
--
-- ==== __Examples__
--
-- @
-- 'shadowColor' \"red\"
-- 'shadowColor' \"#00FF00\"
-- @
shadowColor :: ST.Text -> Canvas ()
shadowColor = Generated.shadowColor . LT.fromStrict

-- | Adds a color and stop position in a 'CanvasGradient'. A stop position is a
-- number between 0.0 and 1.0 that represents the position between start and stop
-- in a gradient.
--
-- ==== __Example__
--
-- @
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- grd # 'addColorStop'(0, \"red\")
-- @
addColorStop :: (Interval, ST.Text) -> CanvasGradient -> Canvas ()
addColorStop (i, t) = Canvas.addColorStop (i, LT.fromStrict t)

-- | Change the canvas cursor to the specified URL or keyword.
--
-- ==== __Examples__
--
-- @
-- cursor \"url(image.png), default\"
-- cursor \"crosshair\"
-- @
cursor :: ST.Text -> Canvas ()
cursor = Canvas.cursor . LT.fromStrict

-- | The height of an 'Image' in pixels.
height :: (Image image, Num a) => image -> a
height = JavaScript.height

-- | The width of an 'Image' in pixels.
width :: (Image image, Num a) => image -> a
width = JavaScript.width

-- | The total length of the audio file in seconds.
durationAudio :: (Audio a, Fractional b) => a -> b
durationAudio = JavaScript.durationAudio

-- | Returns the index of the given Audio in the array of Audio's in the javascript
indexAudio :: Audio a => a -> Int
indexAudio = JavaScript.indexAudio
