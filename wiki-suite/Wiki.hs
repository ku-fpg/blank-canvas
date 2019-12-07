{-# LANGUAGE OverloadedStrings #-}
-- wiki generator support

module Wiki where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Monad as M

import           Data.Text (Text)
import           Data.Time.Clock.POSIX

import           Graphics.Blank

import           System.Directory
import           System.Exit
import           System.IO.Unsafe
import           System.Posix.Process
import           System.Process

import           Text.Printf

import           Development.Shake(removeFiles)

-- import           Trace.Hpc.Reflect
-- import           Trace.Hpc.Tix

snapShot :: DeviceContext -> FilePath -> IO ()
snapShot context fileName = do
        txt <- send context $ do

                tempCanvas <- newCanvas (round (width context + 20  :: Double),
                                         round (height context + 20 :: Double))

                top' <- myCanvasContext

                with tempCanvas $ do
                        -- print a border, because we can (looks better in wiki)

                        save()
                        beginPath()
                        moveTo(1,1)
                        lineTo(width context+1,1)
                        lineTo(width context+1,height context+1)
                        lineTo(1,height context+1)
                        closePath()
                        fillStyle "white"

                        shadowOffsetX 10
                        shadowOffsetY 10;
                        shadowBlur 15;
                        shadowColor "#999";

                        fill()

                        beginPath()
                        moveTo(0.5,0.5)
                        lineTo(width context+1.5,0.5)
                        lineTo(width context+1.5,height context+1.5)
                        lineTo(0.5,height context+1.5)
                        closePath()

                        shadowOffsetX 0
                        shadowOffsetY 0
                        shadowBlur 0
                        lineWidth 1
                        strokeStyle "black"

                        stroke()

                        restore()
                        
                        drawImage(top',[1,1])
                        toDataURL() -- of tempCanvas

        createDirectoryIfMissing True $ "blank-canvas.wiki/tmp"
        writeDataURL ("blank-canvas.wiki/" ++ fileName) txt

wiki :: a -> a
wiki = id

close :: DeviceContext -> IO ()
close context = do
--        n <- getPOSIXTime                
--        Tix tix <- examineTix
--	let tix' = filter (\ t -> ("Graphics.Blank" `isPrefixOf` tixModuleName t)) 
--	         $ tix
--        writeFile ("tix/tix_" ++ printf "_%013d" (floor (fromRational (toRational n) * 1000) :: Integer) ++ ".tix") $ show $ Tix tix'
        send context $ eval "open(location, '_self').close()"
        threadDelay (1000 * 1000);
        putStrLn "dieing"
        p <- getProcessID 
        callProcess "kill" [show p]
        quit

quit :: IO a
quit = exitSuccess

whenM :: Monad m => Bool -> m () -> m ()
whenM = M.when

anim_png :: String -> IO String
anim_png nm = do
   removeFiles "blank.canvas.wiki/tmp" ["*.png"] 
   n <- getPOSIXTime                
   return $ "tmp/" ++ nm ++ printf "_%013d" (floor (fromRational (toRational n) * 1000 :: Double) :: Integer) ++ ".png"

build_anim :: String -> Int -> IO ()
build_anim nm pz = do
       callCommand $ "convert -delay " ++ show pz ++ " -loop 0 -dispose background blank-canvas.wiki/tmp/" ++ nm ++ "_*.png blank-canvas.wiki/images/" ++ nm ++ ".gif"
       return ()


{-# NOINLINE count #-}
count :: TVar Int
count = unsafePerformIO $ newTVarIO 1

counter :: (Int -> Bool) -> (Int -> IO ()) -> IO ()
counter p k = do
    n <- atomically $ do
           v <- readTVar count
           writeTVar count $! v + 1
           return v
    if p n then k n else return ()


ev :: DeviceContext -> EventName -> Int -> IO ()
ev context t c = send context $ trigger $ Event { eMetaKey = False, ePageXY = Nothing, eType = t, eWhich = Just c }
