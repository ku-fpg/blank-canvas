{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures #-}

module Graphics.Blank
        ( module Graphics.Blank
        , module Graphics.Blank.Events
        , module Graphics.Blank.Context
        , module Graphics.Blank.Canvas
        ) where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Web.Scotty as S
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T

import qualified Data.Map as Map

import Graphics.Blank.Events
import Graphics.Blank.Context
import Graphics.Blank.Canvas
import Paths_blank_canvas

-- $(deriveJSON Prelude.id ''Event)

blankCanvas :: Int -> (Context -> IO ()) -> IO ()
blankCanvas port actions = do
   picture <- newEmptyMVar

   dims <- newEmptyMVar
   callbacks <- newMVar $ Map.empty

   -- perform the canvas writing actions
   -- in worker thread.
   forkIO $ do
       -- do not start until we know the screen size
       (w,h) <- takeMVar dims
       actions (Context (w,h) picture callbacks)

   indexHtml <- getDataFileName "static/index.html"

   print indexHtml

   scotty port $ do
--        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ file indexHtml

        post "/start" $ do
            req <- jsonData
            liftIO $ tryPutMVar dims (req  :: (Float,Float))
            json ()

        post "/event" $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            NamedEvent nm event <- jsonData
            db <- liftIO $ readMVar callbacks
            liftIO $ print (nm,event)
            case Map.lookup nm db of
              Nothing -> json ()
              Just var -> do liftIO $ writeEventQueue var event -- perhaps use Chan?
                             json ()

        get "/canvas" $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            res <- liftIO $ tryTakeMVar picture
            case res of
              Just js -> do
                      text $ T.pack js
              Nothing -> do
                 -- hack, wait a second
                 liftIO $ threadDelay (1000 * 1000)
                 text (T.pack "redraw();")


-- | Sends a set of Canvas commands to the canvas. Attempts
-- to common up as many commands as possible.
send :: Context -> Canvas a -> IO a
send cxt@(Context (h,w) var callbacks) commands = send' commands id
  where
      send' :: Canvas a -> (String -> String) -> IO a

      send' (Bind (Return a) k)    cmds = send' (k a) cmds
      send' (Bind (Bind m k1) k2)  cmds = send' (Bind m (\ r -> Bind (k1 r) k2)) cmds
      send' (Bind (Command cmd) k) cmds = send' (k ()) (cmds . shows cmd)
      send' (Bind Size k)          cmds = send' (k (h,w)) cmds
      send' (Bind other k)         cmds = do
              res <- send' other cmds
              send' (k res) id

      send' (Get a op)             cmds = do
              -- clear the commands
              sendToCanvas cxt cmds
              -- get the channel for this event
              chan <- eventChan cxt a
              op chan

      send' (Return a)             cmds = do
              sendToCanvas cxt cmds
              return a
      send' other                  cmds = send' (Bind other Return) cmds

