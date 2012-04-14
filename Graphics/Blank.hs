{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Graphics.Blank where

import Control.Concurrent
import Control.Monad.IO.Class (liftIO)
import Web.Scotty
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import qualified Data.Text.Lazy as T
import Numeric

import Data.Aeson.TH (deriveJSON)

data Canvas = Canvas Int Int (MVar String)

type Dimensions = (Int, Int)

type Point = (Int, Int)
data JsCommand = NoOp | DrawLine Point Point -- | ...
$(deriveJSON Prelude.id ''JsCommand)

blankCanvas :: Int -> (Canvas -> IO ()) -> IO ()
blankCanvas port actions = do
   picture <- newEmptyMVar

   -- perform the canvas writing actions
   -- in worker thread.
   forkIO $ actions (Canvas 800 600 picture)

   dims <- newMVar (100 :: Int, 100 :: Int)

   scotty port $ do
--        middleware logStdoutDev
        middleware $ staticRoot "static"

        get "/" $ file "static/index.html"
{-
        post "/setDims" $ do
            req <- jsonData
            liftIO $ modifyMVar_ dims (const (return req))
            json ()
-}
        get "/canvas" $ do
            header "Cache-Control" "max-age=0, no-cache, private, no-store, must-revalidate"
            -- do something and return a new list of commands to the client
            res <- liftIO $ tryTakeMVar picture
            case res of
              Just js -> text ("var c = getContext();" `T.append` T.pack js)
              Nothing -> text (T.pack "redraw();")


        get "/poll" $ do
            -- do something and return a new list of commands to the client
--            liftIO $ do
--                res <- tryTakeMVar
            json [NoOp,DrawLine (5,5) (50,50)]

width :: Canvas -> Float
width (Canvas w _ _) = fromIntegral w

height :: Canvas -> Float
height (Canvas _ h _) = fromIntegral h

send :: Canvas -> [Command] -> IO ()
send (Canvas _ _ var) commands = do

    putMVar var $ unlines $
        [ "var c = getContext(); "
        ] ++ map show commands
          ++ [ "redraw();" ]

    return ()

wait :: Canvas -> IO ()
wait (Canvas _ _ var)  = do

    putMVar var $ unlines $
        [ ]

    return ()

data Command
        = BeginPath
        | ClearRect Float Float Float Float
        | ClosePath
        | Fill
        | FillStyle String
        | LineTo Float Float
        | LineWidth Float
        | MiterLimit Float
        | MoveTo Float Float
        | Restore
        | Rotate Float
        | Scale Float Float
        | Save
        | Stroke
        | StrokeStyle String
        | Transform Float Float Float Float Float Float
        | Translate Float Float

rgba :: (Int,Int,Int,Int) -> String
rgba (r,g,b,a) = "rgba(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ ")"

showJ a = showFFloat (Just 3) a ""

instance Show Command where
  show (BeginPath)             = "c.beginPath();"
  show (ClosePath)             = "c.closePath();"
  show (ClearRect a b c d)     = "c.clearRect(" ++ showJ a ++ "," ++ showJ b ++ "," ++ showJ c ++ "," ++ showJ d ++ ");"
  show (Fill)                  = "c.fill();"
  show (FillStyle any)         = "c.fillStyle = " ++ show any ++ ";"
  show (LineTo a b)            = "c.lineTo(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (LineWidth w)           = "c.lineWidth = " ++ showJ w ++ ";"
  show (MiterLimit f)          = "c.miterLimit = " ++ showJ f ++ ";"
  show (MoveTo a b)            = "c.moveTo(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (Restore)   	       = "c.restore();"
  show (Rotate f)   	       = "c.rotate(" ++ showJ f ++ ");"
  show (Scale a b)             = "c.scale(" ++ showJ a ++ "," ++ showJ b ++ ");"
  show (Save) 		       = "c.save();"
  show (Stroke)   	       = "c.stroke();"
  show (StrokeStyle any)       = "c.strokeStyle = " ++ show any ++ ";"
  show (Transform a b c d e f) = "c.transform(" ++ showJ a ++ "," ++ showJ b ++ "," ++ showJ c ++ "," ++ showJ d ++ "," ++ showJ e ++ "," ++ showJ f ++ ");"
  show (Translate a b)         = "c.translate(" ++ showJ a ++ "," ++ showJ b ++ ");"

-- And our Deep DSL.

beginPath = BeginPath
closePath = ClosePath
clearRect (a,b,c,d) = ClearRect a b c d

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
rotate = Rotate
scale (a,b) = Scale a b
save = Save
stroke = Stroke
transform (a,b,c,d,e,f) = Transform a b c d e f
translate (a,b) = Translate a b

