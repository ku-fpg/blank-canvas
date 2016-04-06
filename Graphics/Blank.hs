{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

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
        , DeviceContext       -- abstact
        , send
        , sendW
        , sendS
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

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad (forever)
import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Aeson.Types (parse)
import           Data.List as L
import qualified Data.Map as M (lookup)
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import           Data.Text.Lazy (Text)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text as ST

import qualified Graphics.Blank.Canvas as Canvas
import           Graphics.Blank.Canvas hiding (addColorStop, cursor)
import           Graphics.Blank.DeviceContext
import           Graphics.Blank.Events
import qualified Graphics.Blank.Generated as Generated
import           Graphics.Blank.Generated hiding (fillStyle, font, strokeStyle, shadowColor)
import qualified Graphics.Blank.JavaScript as JavaScript
import           Graphics.Blank.JavaScript hiding (width, height, durationAudio, indexAudio)
import           Graphics.Blank.Types
import           Graphics.Blank.Utils

import           Graphics.Blank.GenSym (runGenSym)
import           Graphics.Blank.Instr

import qualified Network.HTTP.Types as H
import           Network.Mime (defaultMimeMap, fileNameExtensions)
import           Network.Wai (Middleware, responseLBS)
import           Network.Wai.Middleware.Local
import           Network.Wai.Handler.Warp
-- import           Network.Wai.Middleware.RequestLogger -- Used when debugging
-- import           Network.Wai.Middleware.Static

import           Paths_blank_canvas

import           Prelude.Compat hiding ((.), id)
import           Control.Category

import           System.IO.Unsafe (unsafePerformIO)
-- import           System.Mem.StableName

-- import           TextShow (Builder, showb, showt)

import qualified Web.Scotty as Scotty
import           Web.Scotty (scottyApp, get, file)
import qualified Web.Scotty.Comet as KC

import           Control.Natural
import qualified Control.Natural as N
import           Control.Remote.Monad
import qualified Control.Remote.Monad.Packet.Weak as WP
import qualified Control.Remote.Monad.Packet.Strong as SP


import           Control.Monad.Reader hiding (local)
import           Control.Monad.State  (StateT, modify, runStateT, evalStateT)
import qualified Control.Monad.State as State
import           Control.Monad.Writer

import           Control.Monad.Trans.Free
import           Control.Monad.Identity

import           Data.String

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

   -- use the comet
   let kc_opts :: KC.Options
       kc_opts = KC.Options { KC.prefix = "/blank", KC.verbose = if debug opts then 3 else 0 }

   connectApp <- KC.connect kc_opts $ \ kc_doc -> do
       -- register the events we want to watch for
       KC.send kc_doc $ fromString $ unlines $ map toString
          [ "register(" <> showi nm <> ");"
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
       -- let Canvas rm0 = device
       --     rm1 = runReaderT (runStateT rm0 0) (deviceCanvasContext cxt0)
       -- (DeviceAttributes w h dpr, n) <- N.run (runMonad (nat (sendW' cxt0))) rm1
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

   app <- scottyApp $ do
--        middleware logStdoutDev
        sequence_ [ Scotty.middleware ware
                  | ware <- middleware opts
                  ]

        connectApp

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
            Scotty.setHeader "Content-Type" $ mime
            file $ (root opts ++ "/" ++ T.unpack fileName)
          else do
            Scotty.next

        return ()

   runSettings (setPort (port opts)
               $ setTimeout 5
               $ defaultSettings
               ) app

generalSend :: RunMonad m => (DeviceContext -> m Cmd Proc :~> IO) -> DeviceContext -> Canvas a -> IO a
generalSend n cxt (Canvas c) =
    -- XXX: Is it ok to hardcode 0 as the start value here?
  N.run (runMonadT (n cxt))
        (mapRemoteT (runGenSym 0)
                    (runReaderT c
                                (deviceCanvasContext cxt)))

sendS, sendW :: DeviceContext -> Canvas a -> IO a
sendS = generalSend (\cxt -> nat (sendS' cxt))

sendW = generalSend (\cxt -> nat (sendW' cxt))

sendS' :: DeviceContext -> SP.StrongPacket Cmd Proc a -> IO a
sendS' cxt sp = evalStateT (go sp) mempty
  where
    go :: SP.StrongPacket Cmd Proc a -> StateT Instr IO a
    go (SP.Command cmd rest) = do
      case cmd of
        Method m canvasCxt -> modify (<> jsCanvasContext canvasCxt <> singleton '.' <> showi m <> singleton ';')
        Canvas.Command c _ -> modify (<> showi cmd <> singleton ';')
        MethodAudio    a _ -> modify (<> showi cmd <> singleton ';')
        Function f r c     -> sendFunc f r c
      go rest

    go (SP.Procedure p) =
      case p of
        Query    q c -> sendQuery q c

    go SP.Done = do
      cmds <- State.get
      liftIO $ sendToCanvas cxt cmds
      State.put mempty
      return ()

    sendFunc :: Function a -> a -> CanvasContext -> StateT Instr IO ()
    sendFunc q@(CreateLinearGradient _) r c = sendGradient q r c
    sendFunc q@(CreateRadialGradient _) r c = sendGradient q r c
    sendFunc q@(CreatePattern        _) r c = sendPattern  q r c

    fileQuery :: Text -> IO ()
    fileQuery url = do
        let url' = if "/" `T.isPrefixOf` url then T.tail url else url
        atomically $ do
            db <- readTVar (localFiles cxt)
            writeTVar (localFiles cxt) $ S.insert url' $ db

    -- TODO: See if the query should be added to the end and then
    -- everything sent over in one transmission.
    sendQuery :: Query a -> CanvasContext -> StateT Instr IO a
    sendQuery query c = do
        -- Grab waiting commands and clear the command queue
      prevCmds <- State.get
      State.put mempty

        -- Send actual query
      liftIO $ do
        case query of
          NewImage url -> fileQuery url
          NewAudio url -> fileQuery url
          _            -> return ()

        -- send the com
        uq <- atomically getUniq
        -- The query function returns a function takes the unique port number of the reply.
        sendToCanvas cxt $ prevCmds <> showi query <> singleton '(' <> showi uq <> singleton ',' <> jsCanvasContext c <> ");"
        v <- KC.getReply (theComet cxt) uq
        case parse (parseQueryResult query) v of
          Error msg -> fail msg
          Success a -> return a

    sendGradient :: Function CanvasGradient -> CanvasGradient -> CanvasContext -> StateT Instr IO ()
    sendGradient q r@(CanvasGradient gId) c = do
      modify (<> "var gradient_"
          <> showi gId     <> " = "   <> jsCanvasContext c
          <> singleton '.' <> showi q <> singleton ';')

    sendPattern :: Function CanvasPattern -> CanvasPattern -> CanvasContext -> StateT Instr IO ()
    sendPattern q r@(CanvasPattern pId) c = do
      modify (<> "var pattern_"
          <> showi pId     <> " = "   <> jsCanvasContext c
          <> singleton '.' <> showi q <> singleton ';')


sendW' :: DeviceContext -> WP.WeakPacket Cmd Proc a -> IO a
sendW' cxt = go mempty
  where
    go :: Instr -> WP.WeakPacket Cmd Proc a -> IO a
    go cmds (WP.Command cmd) =
      case cmd of
        Method m canvasCxt -> send' (cmds <> jsCanvasContext canvasCxt <> singleton '.' <> showi m <> singleton ';')
        Canvas.Command c _ -> send' (cmds <> showi cmd <> singleton ';')
        MethodAudio a    _ -> send' (cmds <> showi cmd <> singleton ';')
        Function f r c -> sendFunc cmds f r c

    go cmds (WP.Procedure p) =
      case p of
        Query    q c -> sendQuery cmds q c

    send' :: Instr -> IO ()
    send' = sendToCanvas cxt

    sendFunc :: Instr -> Function a -> a -> CanvasContext -> IO ()
    sendFunc cmds q@(CreateLinearGradient _) r c = sendGradient cmds q r c
    sendFunc cmds q@(CreateRadialGradient _) r c = sendGradient cmds q r c
    sendFunc cmds q@(CreatePattern        _) r c = sendPattern  cmds q r c

    fileQuery :: Text -> IO ()
    fileQuery url = do
        let url' = if "/" `T.isPrefixOf` url then T.tail url else url
        atomically $ do
            db <- readTVar (localFiles cxt)
            writeTVar (localFiles cxt) $ S.insert url' $ db

    sendQuery :: Instr -> Query a -> CanvasContext -> IO a
    sendQuery cmds query c = do
      case query of
        NewImage url -> fileQuery url
        NewAudio url -> fileQuery url
        -- TODO: See if this is possible:
        Frame        -> error "Frame not yet implemented for weak send"
        _            -> return ()

      -- send the com
      uq <- atomically getUniq
      -- The query function returns a function takes the unique port number of the reply.
      send' $ cmds <> showi query <> singleton '(' <> showi uq <> singleton ',' <> jsCanvasContext c <> ");"
      v <- KC.getReply (theComet cxt) uq
      case parse (parseQueryResult query) v of
        Error msg -> fail msg
        Success a -> return a

    sendGradient :: Instr -> Function CanvasGradient -> CanvasGradient -> CanvasContext -> IO ()
    sendGradient cmds q r@(CanvasGradient gId) c = do
      send' $ cmds <> "var gradient_"
          <> showi gId     <> " = "   <> jsCanvasContext c
          <> singleton '.' <> showi q <> singleton ';'

    sendPattern :: Instr -> Function CanvasPattern -> CanvasPattern -> CanvasContext -> IO ()
    sendPattern cmds q r@(CanvasPattern pId) c = do
      send' $ cmds <> "var pattern_"
          <> showi pId     <> " = "   <> jsCanvasContext c
          <> singleton '.' <> showi q <> singleton ';'

-- | Sends a set of canvas commands to the 'Canvas'. Attempts
-- to common up as many commands as possible. Should not crash.
send :: DeviceContext -> Canvas a -> IO a
send = sendS


-- send :: DeviceContext -> Canvas a -> IO a
-- -- send _   (Return a) = return a
-- -- send cxt (Bind m k)          | weakRemoteMonad cxt = send cxt m >>= send cxt . k
-- -- send cxt (With c (Bind m k)) | weakRemoteMonad cxt = send cxt (With c m) >>= send cxt . With c . k
-- send cxt = N.run $ runMonad (nat go)
--   where
--     go :: WP.WeakPacket Cmd Proc a -> IO a
--     -- TODO: See if this optimization can be done with remote-monad:
--     -- go (WP.Procedure (With _ (WP.Procedure (With c m)))) | weakRemoteMonad cxt = go (With c m)
--     go commands = send' (deviceCanvasContext cxt) commands mempty
--       where
--           sendBind :: CanvasContext -> Canvas a -> (a -> Canvas b) -> Builder -> IO b
--           -- sendBind c (Return a)        k cmds = send' c (k a) cmds
--           -- sendBind c (Bind m k1)      k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
--           sendBind c (Method cmd)      k cmds = send' c (k ()) (cmds <> jsCanvasContext c <> singleton '.' <> showb cmd <> singleton ';')
--           sendBind c (Command cmd)     k cmds = send' c (k ()) (cmds <> showb cmd <> singleton ';')
--           sendBind c (Function func)   k cmds = sendFunc c func k cmds
--           sendBind c (Query query)     k cmds = sendQuery c query k cmds
--           sendBind c (With c' m)       k cmds = send' c' (m >>= (With c . k)) cmds
--           sendBind c (MethodAudio cmd) k cmds = send' c (k ()) (cmds <> showb cmd <> singleton ';')
--           sendBind c MyContext         k cmds = send' c (k c) cmds

--           sendFunc :: CanvasContext -> Function a -> (a -> Canvas b) -> Builder -> IO b
--           sendFunc c q@(CreateLinearGradient _) k cmds = sendGradient c q k cmds
--           sendFunc c q@(CreateRadialGradient _) k cmds = sendGradient c q k cmds
--           sendFunc c q@(CreatePattern        _) k cmds = sendPattern  c q k cmds

--           sendGradient :: CanvasContext -> Function a -> (CanvasGradient -> Canvas b) -> Builder -> IO b
--           sendGradient c q k cmds = do
--             gId <- atomically getUniq
--             send' c (k $ CanvasGradient gId) $ cmds <> "var gradient_"
--                 <> showb gId     <> " = "   <> jsCanvasContext c
--                 <> singleton '.' <> showb q <> singleton ';'

--           sendPattern :: CanvasContext -> Function a -> (CanvasPattern -> Canvas b) -> Builder -> IO b
--           sendPattern c q k cmds = do
--             pId <- atomically getUniq
--             send' c (k $ CanvasPattern pId) $ cmds <> "var pattern_"
--                 <> showb pId     <> " = "   <> jsCanvasContext c
--                 <> singleton '.' <> showb q <> singleton ';'

--           fileQuery :: Text -> IO ()
--           fileQuery url = do
--               let url' = if "/" `T.isPrefixOf` url then T.tail url else url
--               atomically $ do
--                   db <- readTVar (localFiles cxt)
--                   writeTVar (localFiles cxt) $ S.insert url' $ db

--           sendQuery :: CanvasContext -> Query a -> (a -> Canvas b) -> Builder -> IO b
--           sendQuery c query k cmds = do
--               case query of
--                 NewImage url -> fileQuery url
--                 NewAudio url -> fileQuery url
--                 _ -> return ()

--               -- send the com
--               uq <- atomically $ getUniq
--               -- The query function returns a function takes the unique port number of the reply.
--               sendToCanvas cxt $ cmds <> showb query <> singleton '(' <> showb uq <> singleton ',' <> jsCanvasContext c <> ");"
--               v <- KC.getReply (theComet cxt) uq
--               case parse (parseQueryResult query) v of
--                 Error msg -> fail msg
--                 Success a -> send' c (k a) mempty

--           send' :: CanvasContext -> Canvas a -> Builder -> IO a
--           -- Most of these can be factored out, except return
--           -- send' c (Bind m k)            cmds = sendBind c m k cmds
--           send' _ (With c m)            cmds = send' c m cmds  -- This is a bit of a hack
--           -- send' _ (Return a)            cmds = do
--           --         sendToCanvas cxt cmds
--           --         return a
--           send' c cmd                   cmds = sendBind c cmd return cmds

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
