{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
          -- ** 'send'ing to the Graphics 'DeviceContext'
        , DeviceContext       -- abstract
        , send
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
        , GradientCommand
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
         -- * Transformations
         -- $transformations
        , Transformation(..)
        -- * @blank-canvas@ Extensions
        -- ** Reading from 'Canvas'
        , newImage
        , CanvasImage -- abstract
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
         -- ** Drawing Utilities
        , module Graphics.Blank.Utils
         -- ** Events
        , trigger
        , eventQueue
        , wait
        , flush
        , Event(..)
        , EventName
        , EventQueue
        -- ** Cursor manipulation
        , cursor
        -- ** Middleware
        , local_only
        ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Control.Transformation (Transformation(..))

import           Data.Aeson
import           Data.Aeson.Types (parse)
import           Data.List as L
import qualified Data.Map as M (lookup)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LT

import qualified Graphics.Blank.Canvas as Canvas
import           Graphics.Blank.Canvas hiding (addColorStop, cursor)
import           Graphics.Blank.DeviceContext
import           Graphics.Blank.Events
import qualified Graphics.Blank.Generated as Generated
import           Graphics.Blank.Generated hiding (fillStyle, font, strokeStyle, shadowColor)
import qualified Graphics.Blank.JavaScript as JavaScript
import           Graphics.Blank.JavaScript hiding (width, height)
import           Graphics.Blank.Types
import           Graphics.Blank.Utils

import qualified Network.HTTP.Types as H
import           Network.Mime (defaultMimeMap, fileNameExtensions)
import           Network.Wai (Middleware, responseLBS)
import           Network.Wai.Middleware.Local
import           Network.Wai.Handler.Warp
-- import           Network.Wai.Middleware.RequestLogger -- Used when debugging
-- import           Network.Wai.Middleware.Static

import           Paths_blank_canvas

import           Prelude.Compat hiding (show)

import           System.IO.Unsafe (unsafePerformIO)
-- import           System.Mem.StableName

import qualified Text.Show as S (show)
import qualified Text.Show.Text as T (show)
import           Text.Show.Text (Builder, showb, singleton)

import qualified Web.Scotty as Scotty
import           Web.Scotty (scottyApp, get, file)
import qualified Web.Scotty.Comet as KC

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

--   print dataDir

   app <- scottyApp $ do
--        middleware logStdoutDev
        sequence_ [ Scotty.middleware ware
                  | ware <- middleware opts
                  ]
        -- use the comet
        let kc_opts :: KC.Options
            kc_opts = KC.Options { KC.prefix = "/blank", KC.verbose = if debug opts then 3 else 0 }



        KC.connect kc_opts $ \ kc_doc -> do
                -- register the events we want to watch for
                KC.send kc_doc $ T.unlines
                   [ "register(" <> T.show nm <> ");"
                   | nm <- events opts
                   ]

                queue <- atomically newTChan
                _ <- forkIO $ forever $ do
                        val <- atomically $ readTChan $ KC.eventQueue $ kc_doc
                        case fromJSON val of
                           Success (event :: Event) -> do
                                   atomically $ writeTChan queue event
                           _ -> return ()


                let cxt0 = DeviceContext kc_doc queue 300 300 1 locals False

                -- A bit of bootstrapping
                DeviceAttributes w h dpr <- send cxt0 device
                -- print (DeviceAttributes w h dpr)

                let cxt1 = cxt0
                         { ctx_width = w
                         , ctx_height = h
                         , ctx_devicePixelRatio = dpr
                         , weakRemoteMonad = weak opts
                         }

                (actions $ cxt1) `catch` \ (e :: SomeException) -> do
                        print ("Exception in blank-canvas application:" :: String)
                        print e
                        throw e

        get "/"                 $ file $ dataDir ++ "/static/index.html"
        get "/jquery.js"        $ file $ dataDir ++ "/static/jquery.js"
        get "/jquery-json.js"   $ file $ dataDir ++ "/static/jquery-json.js"
        get "/kansas-comet.js"  $ file $ kComet

        -- There has to be a better way of doing this, using function, perhaps?
        get (Scotty.regex "^/(.*)$") $ do
          fileName :: Text <- Scotty.param "1"
          db <- liftIO $ atomically $ readTVar $ locals
          if fileName `S.member` db
          then do
            let mime = mimeType fileName
            Scotty.setHeader "Content-Type" $ LT.fromStrict $ mime
            file $ (root opts ++ "/" ++ T.unpack fileName)
          else do
            Scotty.next

        return ()

   runSettings (setPort (port opts)
               $ setTimeout 5
               $ defaultSettings
               ) app

-- | Sends a set of canvas commands to the 'Canvas'. Attempts
-- to common up as many commands as possible. Should not crash.
--
-- Note that
-- 
-- @
-- 'send' context canvas
-- @
--
-- is equivalent to
--
-- @
-- context # canvas
-- @
send :: DeviceContext -> Canvas a -> IO a
send _   (Return a) = return a
send cxt (Bind m k)          | weakRemoteMonad cxt = send cxt m >>= send cxt . k
send cxt (With c (Bind m k)) | weakRemoteMonad cxt = send cxt (With c m) >>= send cxt . With c . k
send cxt (With _ (With c m)) | weakRemoteMonad cxt = send cxt (With c m)
send cxt commands = send' (deviceCanvasContext cxt) commands mempty
  where
      sendBind :: CanvasContext -> Canvas a -> (a -> Canvas b) -> Builder -> IO b
      sendBind c (Return a)      k cmds = send' c (k a) cmds
      sendBind c (Bind m k1)    k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
      sendBind c (Method cmd)    k cmds = send' c (k ()) (cmds <> jsCanvasContext c <> singleton '.' <> showb cmd <> singleton ';')
      sendBind c (Command cmd)   k cmds = sendCommand c cmd k cmds
      sendBind c (Function func) k cmds = sendFunc c func k cmds
      sendBind c (Query query)   k cmds = sendQuery c query k cmds
      sendBind c (With c' m)     k cmds = send' c' (Bind m (With c . k)) cmds
      sendBind c MyContext       k cmds = send' c (k c) cmds

      sendCommand :: CanvasContext -> Command a -> (a -> Canvas b) -> Builder -> IO b
      -- The only thing that a GradientCommand can possibly return is (). However, we
      -- may want to add other kinds of commands in the future which actually make use
      -- of polymorphism (unlike GradientCommand, which is only polymorphic to make use
      -- a free monad and natural transformation). This approach should be generalizable
      -- to other sorts of commands.
      sendCommand c cmd@(GC _ (GradientCommand gcFree)) k cmds
          = send' c (k (iter (\(AddColorStop _ a) -> a) gcFree)) (cmds <> showb cmd)
      sendCommand c cmd@(Trigger _) k cmds = sendCommandUnit c cmd k cmds
      sendCommand c cmd@(Log     _) k cmds = sendCommandUnit c cmd k cmds
      sendCommand c cmd@(Eval    _) k cmds = sendCommandUnit c cmd k cmds
      
      sendCommandUnit :: CanvasContext -> Command () -> (() -> Canvas b) -> Builder -> IO b
      sendCommandUnit c cmd k cmds = send' c (k ()) (cmds <> showb cmd <> singleton ';')
      
      sendFunc :: CanvasContext -> Function a -> (a -> Canvas b) -> Builder -> IO b
      sendFunc c q@(CreateLinearGradient _) k cmds = sendGradient c q k cmds
      sendFunc c q@(CreateRadialGradient _) k cmds = sendGradient c q k cmds
      sendFunc c q@(CreatePattern        _) k cmds = sendPattern  c q k cmds

      sendGradient :: CanvasContext -> Function a -> (CanvasGradient -> Canvas b) -> Builder -> IO b
      sendGradient c q k cmds = do
        gId <- atomically getUniq
        send' c (k $ CanvasGradient gId) $ cmds <> "var gradient_"
            <> showb gId     <> " = "   <> jsCanvasContext c
            <> singleton '.' <> showb q <> singleton ';'

      sendPattern :: CanvasContext -> Function a -> (CanvasPattern -> Canvas b) -> Builder -> IO b
      sendPattern c q k cmds = do
        pId <- atomically getUniq
        send' c (k $ CanvasPattern pId) $ cmds <> "var pattern_"
            <> showb pId     <> " = "   <> jsCanvasContext c
            <> singleton '.' <> showb q <> singleton ';'

      fileQuery :: Text -> IO ()
      fileQuery url = do
          let url' = if "/" `T.isPrefixOf` url then T.tail url else url
          atomically $ do
              db <- readTVar (localFiles cxt)
              writeTVar (localFiles cxt) $ S.insert url' $ db

      sendQuery :: CanvasContext -> Query a -> (a -> Canvas b) -> Builder -> IO b
      sendQuery c query k cmds = do
          case query of
            NewImage url -> fileQuery url
            NewAudio url -> fileQuery url
            _ -> return ()

          -- send the com
          uq <- atomically $ getUniq
          -- The query function returns a function takes the unique port number of the reply.
          sendToCanvas cxt $ cmds <> showb query <> singleton '(' <> showb uq <> singleton ',' <> jsCanvasContext c <> ");"
          v <- KC.getReply (theComet cxt) uq
          case parse (parseQueryResult query) v of
            Error msg -> fail msg
            Success a -> send' c (k a) mempty

      send' :: CanvasContext -> Canvas a -> Builder -> IO a
      -- Most of these can be factored out, except return
      send' c (Bind m k)            cmds = sendBind c m k cmds
      send' _ (With c m)            cmds = send' c m cmds  -- This is a bit of a hack
      send' _ (Return a)            cmds = do
              sendToCanvas cxt cmds
              return a
      send' c cmd                   cmds = sendBind c cmd Return cmds

instance Transformation Canvas IO DeviceContext where
    (#) = send

local_only :: Middleware
local_only = local $ responseLBS H.status403 [("Content-Type", "text/plain")] "local access only"

{-# NOINLINE uniqVar #-}
uniqVar :: TVar Int
uniqVar = unsafePerformIO $ newTVarIO 0

getUniq :: STM Int
getUniq = do
    u <- readTVar uniqVar
    writeTVar uniqVar (u + 1)
    return u

mimeType :: Text -> Text
mimeType filePath = go $ fileNameExtensions filePath
  where
    go [] = error $ "do not understand mime type for : " ++ S.show filePath
    go (e:es) = case M.lookup e defaultMimeMap of
                     Nothing -> go es
                     Just mt -> decodeUtf8 mt

-------------------------------------------------

-- | Additional @blank-canvas@ settings. The defaults can be used by creating
-- 'Options' as a 'Num'. For example, @'blankCanvas' 3000@ uses the default 'Options'
-- on port 3000.
data Options = Options
        { port   :: Int              -- ^ On which port do we issue @blank-canvas@?
        , events :: [EventName]      -- ^ To which events does the canvas listen? Default: @[]@
        , debug  :: Bool             -- ^ Turn on debugging. Default: @False@
        , root   :: String           -- ^ Location of the static files. Default: @\".\"@
        , middleware :: [Middleware] -- ^ Extra middleware(s) to be executed. Default: @['local_only']@
        , weak       :: Bool         -- ^ use a weak monad, which may help debugging (default False)
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
                            , weak = False
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
fillStyle :: Text -> Canvas ()
fillStyle = Generated.fillStyle

-- | Sets the text context's font properties.
--
-- ==== __Examples__
--
-- @
-- 'font' \"40pt \'Gill Sans Extrabold\'\"
-- 'font' \"80% sans-serif\"
-- 'font' \"bold italic large serif\"
-- @
font :: Text -> Canvas ()
font = Generated.font

-- | Sets the color used for strokes (@\"black\"@ by default).
--
-- ==== __Examples__
--
-- @
-- 'strokeStyle' \"red\"
-- 'strokeStyle' \"#00FF00\"
-- @
strokeStyle :: Text -> Canvas ()
strokeStyle = Generated.strokeStyle

-- | Sets the color used for shadows.
--
-- ==== __Examples__
--
-- @
-- 'shadowColor' \"red\"
-- 'shadowColor' \"#00FF00\"
-- @
shadowColor :: Text -> Canvas ()
shadowColor = Generated.shadowColor

-- | Adds a color and stop position in a 'CanvasGradient'. A stop position is a
-- number between 0.0 and 1.0 that represents the position between start and stop
-- in a gradient.
--
-- ==== __Example__
--
-- @
-- grd <- 'createLinearGradient'(0, 0, 10, 10)
-- grd # do 'addColorStop'(0, \"red\")
--          'addColorStop'(1, \"blue\")
-- @
addColorStop :: (Interval, Text) -> GradientCommand ()
addColorStop = Canvas.addColorStop

-- | Change the canvas cursor to the specified URL or keyword.
--
-- ==== __Examples__
--
-- @
-- cursor \"url(image.png), default\"
-- cursor \"crosshair\"
-- @
cursor :: Text -> Canvas ()
cursor = Canvas.cursor

-- | The height of an 'Image' in pixels.
height :: (Image image, Num a) => image -> a
height = JavaScript.height

-- | The width of an 'Image' in pixels.
width :: (Image image, Num a) => image -> a
width = JavaScript.width

{- $transformations

@blank-canvas@ has several types which represent object types in JavaScript,
such as 'DeviceContext' and 'CanvasGradient'. In JavaScript, functions on these
objects would be invoked by use of the dot operator, e.g., @object.function();@.
The equivalent operator in @blank-canvas@ is (@#@). For example, it can be used
to represent 'send'ing 'Canvas' commands to a 'DeviceContext':

@
blankCanvas 3000 $ \cxt ->
    cxt # do
        moveTo(50,50)
        lineTo(200,100)
        lineWidth 10
        strokeStyle "red"
        stroke()
@

It can also represent adding color stops to a 'CanvasGradient':

@
grd <- createLinearGradient(0, 0, 10, 10)
grd # do addColorStop(0, "red")
         addColorStop(1, "blue")
@

The 'Transformation' class instances represent the relationships between object
types and their function types. For example,

@
instance 'Transformation' 'Canvas' 'IO' 'DeviceContext'
@

indicates that 'DeviceContext' objects can call functions (via @#@) in a 'Canvas'
context and return something in an 'IO' context.

-}

