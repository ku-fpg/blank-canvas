{-|
Module:      Graphics.Blank.GHCi
Copyright:   (C) 2014-2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Beta
Portability: GHC

The GHCi entry point for @blank-canvas@. Useful for sending multiple
commands to the same port.
-}
module Graphics.Blank.GHCi (splatCanvas) where
        
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

import Graphics.Blank (Options(..),port,sendW, Canvas, blankCanvas)
import Graphics.Blank.Canvas

import Control.Natural as N
import Control.Remote.Monad

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Reader (runReaderT)
import Control.Monad.State  (runStateT)
import Graphics.Blank.DeviceContext

-- | splitCanvas is the GHCi entry point into @blank-canvas@.
-- A typical invocation would be
-- 
-- >GHCi> import Graphics.Blank
-- >GHCi> import Graphics.Blank.GHCi
-- >
-- >-- Adding commands to the canvas buffer
-- >GHCi> splatCanvas 3000 $ ( .. canvas commands .. )
-- 
-- The system remembers if it has been called on a specific port before,
-- and if so, uses the previous session.

splatCanvas :: Options -> Canvas () -> IO ()
splatCanvas opts cmds = do
    optCh <- atomically $ do
        ports <- readTVar usedPorts
        case lookup (port opts) ports of
          Just ch -> do putTMVar ch cmds
                        return Nothing
          Nothing -> do ch <- newTMVar cmds
                        writeTVar usedPorts ((port opts,ch):ports)
                        return (Just ch)

    case optCh of
      Nothing -> return ()
      Just ch -> do _ <- forkIO $ blankCanvas opts $ \ cxt -> forever $ do
                           Canvas cmd0 <- atomically $ takeTMVar ch
                           let cmd1 = runReaderT (runStateT cmd0 0) (deviceCanvasContext cxt)
                           N.run (runMonad (nat (sendW cxt))) cmd1    -- run the command
                    return ()

-- common TVar for all ports in use.
{-# NOINLINE usedPorts #-}
usedPorts :: TVar [(Int, TMVar (Canvas ()))]
usedPorts = unsafePerformIO $ newTVarIO []

