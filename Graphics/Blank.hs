{-# LANGUAGE OverloadedStrings, TemplateHaskell, GADTs, KindSignatures, CPP, BangPatterns #-}

module Graphics.Blank
        (
         -- * Starting blank-canvas
          blankCanvas
        , splatCanvas
        -- * Graphics 'Context'
        , Context       -- abstact
        , send
         -- * Drawing pictures using the Canvas DSL
        , Canvas        -- abstact
        , module Graphics.Blank.Generated
         -- * Reading from 'Canvas'
        , size
         -- * Drawing Utilities
        , module Graphics.Blank.Utils
         -- * Event Stuff
        , events
        , wait
        , Event(..)
        , EventName(..)
        , NamedEvent(..)
        , EventQueue
        ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai (Middleware,remoteHost, responseLBS)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr(..))
import System.IO.Unsafe (unsafePerformIO)
--import System.Mem.StableName
import Web.Scotty as S
--import Network.Wai.Middleware.RequestLogger -- Used when debugging
--import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T

import qualified Data.Map as Map
import qualified Data.Set as Set

import Graphics.Blank.Events
import Graphics.Blank.Context
import Graphics.Blank.Canvas
import Graphics.Blank.Generated
import Graphics.Blank.Utils
import Paths_blank_canvas

-- | blankCanvas is the main entry point into blank-canvas.
-- A typical invocation would be
--
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


blankCanvas :: Int -> (Context -> IO ()) -> IO ()
blankCanvas port actions = do
   dataDir <- getDataDir
--   print dataDir


   contextDB <- newMVar $ (Map.empty :: Map.Map Int Context)
   let newContext :: (Float,Float) -> IO Int
       newContext (w,h) = do
            uq <- atomically getUniq
            picture <- newEmptyMVar
            callbacks <- newMVar $ Set.empty
            queue <- atomically newTChan
            let cxt = Context (w,h) picture callbacks queue uq
            db <- takeMVar contextDB
            putMVar contextDB $ Map.insert uq cxt db
            -- Here is where we actually spawn the user code
            _ <- forkIO $ actions cxt
            return uq

   app <- scottyApp $ do
--        middleware logStdoutDev
        middleware local_only

--        middleware $ staticRoot $ TS.pack $ (dataDir ++ "/static")

        get "/" $ file $ dataDir ++ "/static/index.html"
        get "/jquery.js" $ file $ dataDir ++ "/static/jquery.js"
        get "/jquery-json.js" $ file $ dataDir ++ "/static/jquery-json.js"

        post "/start" $ do
            req <- jsonData
            uq  <- liftIO $ newContext req
            text (T.pack $ "session = " ++ show uq ++ ";redraw();")

        post "/event/:num" $ do
            addHeader "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            num <- param "num"
            ne <- jsonData
            db <- liftIO $ readMVar contextDB
            case Map.lookup num db of
               Nothing -> json ()
               Just (Context _ _ _ q _) ->
                                do liftIO $ atomically $ writeTChan q ne
                                   json ()

        get "/canvas/:num" $ do
            addHeader "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            num <- param "num"
--            liftIO $ print (num :: Int)
            let tryPicture picture n = do
                    res <- liftIO $ tryTakeMVar picture
                    case res of
                     Just js -> do
--                            liftIO $ print js
                            text $ T.pack js
                     Nothing | n == 0 ->
                            -- give the browser something to do (approx every second)
                            text (T.pack "")
                     Nothing -> do
                            -- hack, wait a 1/10 of a second
                            liftIO $ threadDelay (100 * 1000)
                            tryPicture picture (n - 1 :: Int)

            db <- liftIO $ readMVar contextDB
            case Map.lookup num db of
               Nothing -> text (T.pack $ "alert('/canvas/, can not find " ++ show num ++ "');")
               Just (Context _ pic _ _ _) -> tryPicture pic 10

   run port app

-- | Sends a set of Canvas commands to the canvas. Attempts
-- to common up as many commands as possible. Can not crash.
send :: Context -> Canvas a -> IO a
send cxt@(Context (h,w) _ _ _ _) commands = 
      send' commands id 
  where
      send' :: Canvas a -> (String -> String) -> IO a
      send' (Bind (Return a) k)    cmds = send' (k a) cmds
      send' (Bind (Bind m k1) k2)  cmds = send' (Bind m (\ r -> Bind (k1 r) k2)) cmds
      send' (Bind (Command cmd) k) cmds = send' (k ()) (cmds . shows cmd)
      send' (Bind Size k)          cmds = send' (k (h,w)) cmds
      send' (Return a)             cmds = do
              sendToCanvas cxt cmds
              return a
      send' cmd                    cmds = send' (Bind cmd Return) cmds


local_only :: Middleware
local_only f r = case remoteHost r of
                   SockAddrInet _  h | h == fromIntegral home
                                    -> f r
#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
                   SockAddrUnix _   -> f r
#endif
                   _                ->  return $ responseLBS H.status403
                                                             [("Content-Type", "text/plain")]
                                                             "local access only"
 where
        home :: Integer
        home = 127 + (256 * 256 * 256) * 1

-- | splitCanvas is the GHCi entry point into blank-canvas.
-- A typical invocation would be
--
-- >
-- >
-- >import Graphics.Blank
-- > -- Adding commands to the canvas buffer
-- >splatCanvas 3000 $ (>> do { .. canvas commands .. })
-- > -- Replacing the buffer with some commands
-- >splatCanvas 3000 $ (\ _ -> do { .. canvas commands .. })


splatCanvas :: Int -> (Canvas () -> Canvas ()) -> IO ()
splatCanvas port cmds = do
    optCh <- atomically $ do
        ports <- readTVar usedPorts
        uq <- getUniq
        case lookup port ports of
          Just ch -> do modifyTVar ch $ \ (_,orig) -> (uq,cmds orig)
                        return Nothing
          Nothing -> do ch <- newTVar (uq,cmds (return ()))
                        writeTVar usedPorts ((port,ch):ports)
                        return (Just ch)

    let full cmd = do
            clearCanvas
            cmd


    case optCh of
      Nothing -> return ()
      Just ch -> do
         let callback uq cxt = do
                (uq',cmd) <- atomically $ do
                        (uq',cmd) <- readTVar ch
                        check (uq' /= uq)     -- must be a new command
                        return (uq',cmd)
                send cxt $ full cmd -- issue the screen command (should check for failure)
                callback uq' cxt
         _ <- forkIO $ blankCanvas port $ callback (-1)
         return ()

-- common TVar for all ports in use.
{-# NOINLINE usedPorts #-}
usedPorts :: TVar [(Int,TVar (Int,Canvas ()))]
usedPorts = unsafePerformIO $ newTVarIO []

{-# NOINLINE uniqVar #-}
uniqVar :: TVar Int
uniqVar = unsafePerformIO $ newTVarIO 0

getUniq :: STM Int
getUniq = do
    u <- readTVar uniqVar
    writeTVar uniqVar (u + 1)
    return u
