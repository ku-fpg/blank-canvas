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

import           Data.Aeson
import           Data.Aeson.Types (parse)
import           Data.Monoid((<>))
import           Data.String
import qualified Data.Text as T
import           Data.Text (Text)

import           Graphics.Blank.Canvas
import           Graphics.Blank.DeviceContext
import           Graphics.Blank.Events
import qualified Graphics.Blank.Generated as Generated
import           Graphics.Blank.Generated hiding (fillStyle,strokeStyle)
import qualified Graphics.Blank.JavaScript as JavaScript
import           Graphics.Blank.JavaScript hiding (width, height)
import           Graphics.Blank.Utils

import qualified Network.HTTP.Types as H
import           Network.Socket (SockAddr(..))
import           Network.Wai (Middleware,remoteHost, responseLBS)
import           Network.Wai.Handler.Warp (run)
-- import           Network.Wai.Middleware.RequestLogger -- Used when debugging
-- import           Network.Wai.Middleware.Static

import           Paths_blank_canvas

import           System.IO.Unsafe (unsafePerformIO)
-- import           System.Mem.StableName

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
                   [ "register(" <> T.pack(show nm) <> ");"
                   | nm <- events opts
                   ]

                queue <- atomically newTChan
                _ <- forkIO $ forever $ do
                        val <- atomically $ readTChan $ KC.eventQueue $ kc_doc
                        case fromJSON val of
                           Success (event :: Event) -> do
                                   atomically $ writeTChan queue event
                           _ -> return ()

                let cxt0 = DeviceContext kc_doc queue 300 300 1
                
                -- A bit of bootstrapping 
                DeviceAttributes w h dpr <- send cxt0 device
                -- print (DeviceAttributes w h dpr)

                let cxt1 = cxt0 
                         { ctx_width = w
                         , ctx_height = h
                         , ctx_devicePixelRatio = dpr
                         }
                
                (actions $ cxt1) `catch` \ (e :: SomeException) -> do
                        print ("Exception in blank-canvas application:"  :: String)
                        print e
                        throw e

        get "/"                 $ file $ dataDir ++ "/static/index.html"
        get "/jquery.js"        $ file $ dataDir ++ "/static/jquery.js"
        get "/jquery-json.js"   $ file $ dataDir ++ "/static/jquery-json.js"
        get "/kansas-comet.js"  $ file $ kComet
        sequence_ [ get (fromString ("/" ++ nm)) $ file $ (root opts ++ "/" ++ nm) | nm <- static opts ]
        return ()

   run (port opts) app

-- | Sends a set of Canvas commands to the canvas. Attempts
-- to common up as many commands as possible. Should not crash.

send :: DeviceContext -> Canvas a -> IO a
send cxt commands = 
      send' (deviceCanvasContext cxt) commands id 
  where
      send' :: CanvasContext -> Canvas a -> (String -> String) -> IO a
      send' c (Bind (Return a) k)    cmds = send' c (k a) cmds
      send' c (Bind (Bind m k1) k2)  cmds = send' c (Bind m (\ r -> Bind (k1 r) k2)) cmds
      send' c (Bind (Method cmd) k)  cmds = send' c (k ()) (cmds . ((showJS c ++ ".") ++) . shows cmd . (";" ++))
      send' c (Bind (Command cmd) k) cmds = send' c (k ()) (cmds . shows cmd . (";" ++))
      send' c (Bind (Query query) k) cmds = do
              -- send the com
              uq <- atomically $ getUniq
              -- The query function returns a function takes the unique port number of the reply.
              sendToCanvas cxt (cmds . ((show query ++ "(" ++ show uq ++ "," ++ showJS c ++ ");") ++))
              v <- KC.getReply (theComet cxt) uq
              case parse (parseQueryResult query) v of
                Error msg -> fail msg
                Success a -> do
                        send' c (k a) id
      send' c (Bind (With c' m) k)  cmds = send' c' (Bind m (With c . k)) cmds
      send' c (Bind MyContext k)    cmds = send' c (k c) cmds
      send' c (Bind (LiftIO io) k)  cmds = do
              a <- io
              send' c (k a) cmds

      send' _ (With c m)            cmds = send' c m cmds
      send' c MyContext             cmds = send' c (Return c) cmds
      send' _ (Return a)            cmds = do
              sendToCanvas cxt cmds
              return a
      send' _ (LiftIO io)           _    = io
      send' c cmd                   cmds = send' c (Bind cmd Return) cmds


local_only :: Middleware
local_only f r k = case remoteHost r of
                   SockAddrInet _  h | h == fromIntegral home
                                    -> f r k
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
                   SockAddrUnix _   -> f r k
#endif
                   _                ->  k $ responseLBS H.status403
                                                        [("Content-Type", "text/plain")]
							 "local access only"
 where
        home :: Integer
        home = 127 + (256 * 256 * 256) * 1


{-# NOINLINE uniqVar #-}
uniqVar :: TVar Int
uniqVar = unsafePerformIO $ newTVarIO 0

getUniq :: STM Int
getUniq = do
    u <- readTVar uniqVar
    writeTVar uniqVar (u + 1)
    return u

-------------------------------------------------

data Options = Options 
        { port   :: Int              -- ^ which port do we issue the blank canvas using
        , events :: [EventName]      -- ^ which events does the canvas listen to
        , debug  :: Bool             -- ^ turn on debugging (default False)
        , static :: [String]         -- ^ path to images, and other static artifacts
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
                            , static = []
                            , root = "."
                            , middleware = [local_only]
                            }


-------------------------------------------------
-- This is the monomorphic version, to stop "ambiguous" errors.

fillStyle :: Text -> Canvas ()
fillStyle = Generated.fillStyle

strokeStyle :: Text -> Canvas ()
strokeStyle = Generated.strokeStyle

height :: (Image image, Num a) => image -> a
height = JavaScript.height

width :: (Image image, Num a) => image -> a
width = JavaScript.width

