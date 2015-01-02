{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import           Control.Concurrent

import           Data.List (nub)
import qualified Data.Text as Text
import           Data.Monoid((<>))

import           Graphics.Blank

data State = State
             { keys :: [Int]    -- key *codes* for pressed keys
             , step :: Int
             }
     deriving Show

main :: IO ()
main = blankCanvas 3000 { events = ["keyup","keydown"] } $ \ context -> loop context (State [] 0)

loop :: DeviceContext -> State -> IO a
loop context state = do
--        threadDelay (1 * 1000 * 10)    -- remove if writing a game
        send context $ do
                let (w,h) = (width context, height context)
                clearRect (0,0,w,h)
                lineWidth 1
                fillStyle "red"
                font "30pt Calibri"
                fillText("Keys currently pressed: " <> Text.pack (show (keys state)),50,50)
                fillText("Counter: " <> Text.pack (show (step state)),50,150)

--        print state

        control context state

control :: DeviceContext -> State -> IO a
control context state = do
        event <- wait context
        let down_keys = case (eType event,eWhich event) of
                          ("keydown",Just c) -> [c]
                          _ -> []
        let up_keys = case (eType event,eWhich event) of
                          ("keyup",Just c) -> [c]
                          _ -> []
        let current_keys = [ k | k <- nub (keys state ++ down_keys), not (k `elem` up_keys) ]
        let state' = state { step = step state + 1, keys = current_keys }
        loop context state'


