{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures, CPP, BangPatterns, ScopedTypeVariables #-}

-- | blank-canvas is a Haskell binding to the complete HTML5 Canvas
--   API. blank-canvas allows Haskell users to write, in Haskell,
--   interactive images onto their web browsers. blank-canvas gives
--   the users a single full-window canvas, and provides many
--   well-documented functions for rendering images.

module Graphics.Blank
        (
         -- * Starting blank-canvas
          blankCanvas
        , Options(..)
          -- ** 'send'ing to the Graphics 'DeviceContext'
        , DeviceContext       -- abstact
        , send
          -- * HTML5 Canvas API
          -- | See <http://www.nihilogic.dk/labs/canvas_sheet/HTML5_Canvas_Cheat_Sheet.pdf> for the JavaScript
          --   version of this API.
        , Canvas        -- abstact
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
        , LineJoinCorner(..)
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
        , TextBaselineAlignment(..)
        , TextMetrics(..)
          -- ** Rectangles
        , clearRect
        , fillRect
        , strokeRect
          -- ** Pixel manipulation
        , getImageData
        , putImageData
        , ImageData(..)
        -- * blank-canvas Extensions
        -- ** Reading from 'Canvas'
        , newImage
        , CanvasImage -- abstract
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
        -- ** Middleware
        , local_only
        ) where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Aeson
import           Data.Aeson.Types (parse)
import           Data.List as L
import           Data.Monoid ((<>))
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Builder hiding (flush)

import qualified Graphics.Blank.Canvas as Canvas
import           Graphics.Blank.Canvas hiding (addColorStop)
import           Graphics.Blank.DeviceContext
import           Graphics.Blank.Events
import qualified Graphics.Blank.Generated as Generated
import           Graphics.Blank.Generated hiding (fillStyle, strokeStyle, shadowColor)
import qualified Graphics.Blank.JavaScript as JavaScript
import           Graphics.Blank.JavaScript hiding (width, height)
import           Graphics.Blank.Utils

import qualified Network.HTTP.Types as H
import           Network.Wai (Middleware, responseLBS)
import           Network.Wai.Middleware.Local
import           Network.Wai.Handler.Warp
-- import           Network.Wai.Middleware.RequestLogger -- Used when debugging
-- import           Network.Wai.Middleware.Static

import           Paths_blank_canvas

import           System.IO.Unsafe (unsafePerformIO)
-- import           System.Mem.StableName

import qualified Text.Show.Text as T
import           Text.Show.Text (showb)

import qualified Web.Scotty as Scotty
import           Web.Scotty (scottyApp, get, file)
import qualified Web.Scotty.Comet as KC

-- | blankCanvas is the main entry point into blank-canvas.
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


                let cxt0 = DeviceContext kc_doc queue 300 300 1 locals

                -- A bit of bootstrapping
                DeviceAttributes w h dpr <- send cxt0 device
                -- print (DeviceAttributes w h dpr)

                let cxt1 = cxt0
                         { ctx_width = w
                         , ctx_height = h
                         , ctx_devicePixelRatio = dpr
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
            mime <- mimeTypes (T.unpack fileName)
            Scotty.setHeader "Content-Type" $ LT.fromStrict $ mime
            file $ (root opts ++ "/" ++ T.unpack fileName)
          else do
            Scotty.next

        return ()

   runSettings (setPort (port opts)
               $ setTimeout 5
               $ defaultSettings
               ) app

-- | Sends a set of Canvas commands to the canvas. Attempts
-- to common up as many commands as possible. Should not crash.

send :: DeviceContext -> Canvas a -> IO a
send cxt commands =
      send' (deviceCanvasContext cxt) commands id
  where
      sendBind :: CanvasContext -> Canvas a -> (a -> Canvas b) -> (Builder -> Builder) -> IO b
      sendBind c (Return a)      k cmds = send' c (k a) cmds
      sendBind c (Bind m k1)    k2 cmds = sendBind c m (\ r -> Bind (k1 r) k2) cmds
      sendBind c (Method cmd)    k cmds = send' c (k ()) (cmds . ((jsCanvasContext c <> singleton '.') <>) . (showb cmd <>) . (singleton ';' <>))
      sendBind c (Command cmd)   k cmds = send' c (k ()) (cmds . (showb cmd <>) . (singleton ';' <>))
      sendBind c (Function func) k cmds = sendFunc c func k cmds
      sendBind c (Query query)   k cmds = sendQuery c query k cmds
      sendBind c (With c' m)     k cmds = send' c' (Bind m (With c . k)) cmds
      sendBind c MyContext       k cmds = send' c (k c) cmds

      sendFunc :: CanvasContext -> Function a -> (a -> Canvas b) -> (Builder -> Builder) -> IO b
      sendFunc c q@(CreateLinearGradient _) k cmds = sendGradient c q k cmds
      sendFunc c q@(CreateRadialGradient _) k cmds = sendGradient c q k cmds

      sendGradient :: CanvasContext -> Function a -> (CanvasGradient -> Canvas b) -> (Builder -> Builder) -> IO b
      sendGradient c q k cmds = do
        gId <- atomically getUniq
        send' c (k $ CanvasGradient gId) (cmds 
          . (("var gradient_" <> showb gId <> " = " <> showbJS c <> singleton '.') <>) 
          . (showb q <>) . (singleton ';' <>))

      sendQuery :: CanvasContext -> Query a -> (a -> Canvas b) -> (Builder -> Builder) -> IO b
      sendQuery c query k cmds = do
          case query of
            NewImage url -> do
              let url' = if "/" `T.isPrefixOf` url then T.tail url else url
              atomically $ do
                  db <- readTVar (localFiles cxt)
                  writeTVar (localFiles cxt) $ S.insert url' $ db
            _ -> return ()

          -- send the com
          uq <- atomically $ getUniq
          -- The query function returns a function takes the unique port number of the reply.
          sendToCanvas cxt (cmds . ((showb query <> singleton '(' <> showb uq <> singleton ',' <> jsCanvasContext c <> ");") <>))
          v <- KC.getReply (theComet cxt) uq
          case parse (parseQueryResult query) v of
            Error msg -> fail msg
            Success a -> do
                    send' c (k a) id

      send' :: CanvasContext -> Canvas a -> (Builder -> Builder) -> IO a
      -- Most of these can be factored out, except return
      send' c (Bind m k)            cmds = sendBind c m k cmds
      send' _ (With c m)            cmds = send' c m cmds  -- This is a bit of a hack
      send' _ (Return a)            cmds = do
              sendToCanvas cxt cmds
              return a
      send' c cmd                   cmds = sendBind c cmd Return cmds

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

mimeTypes :: Monad m => FilePath -> m Text
mimeTypes filePath
  | ".jpg" `L.isSuffixOf` filePath = return "image/jpeg"
  | ".png" `L.isSuffixOf` filePath = return "image/png"
  | ".gif" `L.isSuffixOf` filePath = return "image/gif"
  | otherwise = fail $ "do not understand mime type for : " ++ show filePath

-------------------------------------------------

-- TODO: add extra mime types


data Options = Options
        { port   :: Int              -- ^ which port do we issue the blank canvas using
        , events :: [EventName]      -- ^ which events does the canvas listen to
        , debug  :: Bool             -- ^ turn on debugging (default False)
        , root   :: String           -- ^ location of the static files (default .)
        , middleware :: [Middleware] -- ^ extra middleware(s) to be executed. (default [local_only])
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
                            }

-------------------------------------------------
-- This is the monomorphic version, to stop "ambiguous" errors.

fillStyle :: Text -> Canvas ()
fillStyle = Generated.fillStyle

strokeStyle :: Text -> Canvas ()
strokeStyle = Generated.strokeStyle

shadowColor :: Text -> Canvas ()
shadowColor = Generated.shadowColor

addColorStop :: (Double, Text) -> CanvasGradient -> Canvas ()
addColorStop = Canvas.addColorStop

height :: (Image image, Num a) => image -> a
height = JavaScript.height

width :: (Image image, Num a) => image -> a
width = JavaScript.width
