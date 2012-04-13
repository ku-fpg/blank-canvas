{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.Blank where

import Control.Concurrent
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Data.Aeson.TH (deriveJSON)

data Canvas = Canvas

data Command = NoOp -- | ...
$(deriveJSON Prelude.id ''Command)

blankCanvas :: Int -> (Canvas -> IO ()) -> IO ()
blankCanvas port actions = do
   var <- newMVar "/* empty */"
   scotty port $ do
        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ file "static/index.html"

        get "/poll" $ json [NoOp]
