module Main where

import Graphics.Blank
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace

main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> loop context Map.empty X
data XO = X | O
        deriving (Eq,Ord,Show)

swap :: XO -> XO
swap X = O
swap O = X

loop :: Context -> Map (Int,Int) XO -> XO -> IO ()
loop context board turn = do
--        print board
--        print turn
        (width,height,sz) <- send context $ do
                (width,height) <- size
                clearRect (0,0,width,height)
                beginPath()

                let sz = min width height
                save()
                translate (width / 2,height / 2)
                sequence_ [ do bigLine (-sz * 0.45,n) (sz * 0.45,n)
                               bigLine (n,-sz * 0.45) (n,sz * 0.45)
                          | n <- [-sz * 0.15,sz * 0.15]
                          ]


                sequence_ [ do save()
                               translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                               case Map.lookup (x,y) board of
                                  Just X -> drawX (sz * 0.1)
                                  Just O -> drawO (sz * 0.1)
                                  Nothing -> return ()
                               restore()
                          | x <- [-1,0,1]
                          , y <- [-1,0,1]
                          ]
                restore()
                return (width,height,sz)

        let pointToSq :: (Float,Float) -> Maybe (Int,Int)
            pointToSq (x,y) = do
                    x' <- fd ((x - width / 2) / sz)
                    y' <- fd ((y - height / 2) / sz)
                    return (x',y')

            fd x = 
--                    trace (show ("fx",x,r)) $
                    if r `elem` [-1..1] then Just (signum r) else Nothing
                where r = round (x * 3.3333)

        event <- wait context
--        print event
        case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> loop context board turn
           Just (x',y') -> case pointToSq (fromIntegral x',fromIntegral y') of
                             Nothing -> loop context board turn
                             Just pos -> case Map.lookup pos board of
                                           Nothing -> loop context
                                                            (Map.insert pos turn board)
                                                            (swap turn)
                                                    -- already something here
                                           Just _ -> loop context board turn


xColor = "#ff0000"
oColor = "#00a000"
boardColor = "#000080"

drawX :: Float -> Canvas ()
drawX size = do
        strokeStyle xColor
        lineCap "butt"
        beginPath()
        moveTo(-size,-size)
        lineTo(size,size)
        lineWidth 10
        stroke()
        beginPath()
        moveTo(-size,size)
        lineTo(size,-size)
        lineWidth 10
        stroke()

drawO :: Float -> Canvas ()
drawO radius = do
        beginPath()
        arc(0, 0, radius, 0, 2 * pi, False)
        lineWidth 10
        strokeStyle oColor
        stroke()

bigLine :: (Float,Float) -> (Float,Float) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 20
        strokeStyle boardColor
        lineCap "round"
        stroke()

