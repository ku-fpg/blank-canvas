{-# LANGUAGE OverloadedStrings #-}

module Graphics.Blank where

import Control.Concurrent
import Web.Scotty
import Network.Wai.Middleware.RequestLogger


data Canvas = Canvas

blankCanvas :: Int -> (Canvas -> IO ()) -> IO ()
blankCanvas port actions = do
   var <- newMVar "/* empty */"
   scotty port $ do
        middleware logStdoutDev
--        middleware static

        get "/" $ text "foobar"







