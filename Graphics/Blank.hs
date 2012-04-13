{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.Blank where

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static

import Data.Aeson.TH (deriveJSON)


data Canvas = Canvas (MVar String)

type Dimensions = (Int, Int)

type Point = (Int, Int)
data JsCommand = NoOp | DrawLine Point Point -- | ...
$(deriveJSON Prelude.id ''JsCommand)

blankCanvas :: Int -> (Canvas -> IO ()) -> IO ()
blankCanvas port actions = do
   var <- newEmptyMVar

   forkIO $ actions (Canvas var)

   res <- takeMVar var
   writeFile "OUTPUT" res

   dims <- newMVar (100 :: Int, 100 :: Int)

   scotty port $ do
        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ file "static/index.html"

        post "/setDims" $ do
            req <- jsonData
            liftIO $ modifyMVar_ (dims :: MVar (Int,Int)) (const (return req))
            json ()

        get "/poll" $ do
            -- do something and return a new list of commands to the client
            json [NoOp,DrawLine (5,5) (50,50)]

send :: Canvas -> [Command] -> IO ()
send (Canvas var) commands = do

    putMVar var $ unlines $
        [ "function draw() {  "
        , " var canvas = document.getElementById(\"canvas\");"
        , "  if (canvas.getContext) {"
        , "          var c = canvas.getContext(\"2d\");"
        ] ++ map show commands
          ++ ["}}"]

    return ()

data Command
        = BeginPath
        | ClosePath
        | Fill
        | FillStyle String
        | LineTo Float Float
        | LineWidth Float
        | MiterLimit Float
        | MoveTo Float Float
        | Restore
        | Save
        | Stroke
        | StrokeStyle String
        | Transform Float Float Float Float Float Float


instance Show Command where
  show (BeginPath)             = "c.beginPath();"
  show (ClosePath)             = "c.closePath();"
  show (Fill)                  = "c.fill();"
  show (FillStyle any)         = "c.fillStyle = " ++ show any ++ ";"
  show (LineTo a b)            = "c.lineTo(" ++ show a ++ "," ++ show b ++ ");"
  show (LineWidth w)           = "c.lineWidth = " ++ show w ++ ";"
  show (MiterLimit f)          = "c.miterLimit = " ++ show f ++ ";"
  show (MoveTo a b)            = "c.moveTo(" ++ show a ++ "," ++ show b ++ ");"
  show (Restore)   	       = "c.restore();"
  show (Save) 		       = "c.save();"
  show (Stroke)   	       = "c.stroke();"
  show (StrokeStyle any)       = "c.strokeStyle = " ++ show any ++ ";"
  show (Transform a b c d e f) = "c.transform(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ "," ++ show e ++ "," ++ show f ++ ");"


-- And our Deep DSL.

beginPath = BeginPath
closePath = ClosePath
fill = Fill

fillStyle str = FillStyle str

lineTo (a,b) = LineTo a b

lineWidth w = LineWidth w

miterLimit :: Float -> Command
miterLimit f = MiterLimit f

moveTo (a,b) = MoveTo a b

strokeStyle :: String -> Command
strokeStyle str = StrokeStyle str

restore = Restore
save = Save
stroke = Stroke
transform (a,b,c,d,e,f) = Transform a b c d e f
