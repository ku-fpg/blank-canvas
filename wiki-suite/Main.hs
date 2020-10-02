-- Shake Generator for wiki pages
{-# LANGUAGE CPP #-}
module Main where

import           Prelude                     hiding ((*>))

import           Control.Concurrent

import           Data.Char
import qualified Data.List                   as L

import           Development.Shake           hiding (doesFileExist)
import qualified Development.Shake           as Shake
import           Development.Shake.FilePath

import           System.Directory
import           System.IO
import           System.Process

import           Web.Browser

-- TO test: ghci wiki-suite/Draw_Canvas.hs -idist/build/autogen/:.:wiki-suite
import qualified Arc
import qualified Bezier_Curve
import qualified Bounce
import qualified Circle
import qualified Clipping_Region
import qualified Color_Fill
import qualified Color_Square
import qualified Custom_Shape
import qualified Custom_Transform
import qualified Draw_Canvas
import qualified Draw_Device
import qualified Draw_Image
import qualified Favicon
import qualified Font_Size_and_Style
import qualified Get_Image_Data_URL
import qualified Global_Alpha
import qualified Global_Composite_Operations
import qualified Grayscale
import qualified Image_Crop
import qualified Image_Loader
import qualified Image_Size
import qualified Is_Point_In_Path
import qualified Key_Read
import qualified Line
import qualified Line_Cap
import qualified Line_Color
import qualified Line_Join
import qualified Line_Width
import qualified Linear_Gradient
import qualified Load_Image_Data_URL
import qualified Load_Image_Data_URL_2
import qualified Miter_Limit
import qualified Path
import qualified Pattern
import qualified Quadratic_Curve
import qualified Radial_Gradient
import qualified Rectangle
import qualified Red_Line
import qualified Rotate_Transform
import qualified Rotating_Square
import qualified Rounded_Corners
import qualified Scale_Transform
import qualified Semicircle
import qualified Shadow
import qualified Square
import qualified Text_Align
import qualified Text_Baseline
import qualified Text_Color
import qualified Text_Metrics
import qualified Text_Stroke
import qualified Text_Wrap
import qualified Tic_Tac_Toe
import qualified Translate_Transform

import           System.Environment

main :: IO ()
main = do
     args <- getArgs
     main2 args

main2 :: [String] -> IO ()
main2 ["Arc"] = Arc.main
main2 ["Bezier_Curve"] = Bezier_Curve.main
main2 ["Bounce"] = Bounce.main
main2 ["Circle"] = Circle.main
main2 ["Clipping_Region"] = Clipping_Region.main
main2 ["Color_Fill"] = Color_Fill.main
main2 ["Color_Square"] = Color_Square.main
main2 ["Custom_Shape"] = Custom_Shape.main
main2 ["Draw_Canvas"] = Draw_Canvas.main
main2 ["Draw_Device"] = Draw_Device.main
main2 ["Draw_Image"] = Draw_Image.main
main2 ["Favicon"] = Favicon.main
main2 ["Font_Size_and_Style"] = Font_Size_and_Style.main
main2 ["Get_Image_Data_URL"] = Get_Image_Data_URL.main
main2 ["Global_Alpha"] = Global_Alpha.main
main2 ["Global_Composite_Operations"] = Global_Composite_Operations.main
main2 ["Grayscale"] = Grayscale.main
main2 ["Image_Crop"] = Image_Crop.main
main2 ["Image_Loader"] = Image_Loader.main
main2 ["Miter_Limit"] = Miter_Limit.main
main2 ["Image_Size"] = Image_Size.main
main2 ["Is_Point_In_Path"] = Is_Point_In_Path.main
main2 ["Key_Read"] = Key_Read.main
main2 ["Line"] = Line.main
main2 ["Line_Cap"] = Line_Cap.main
main2 ["Line_Color"] = Line_Color.main
main2 ["Line_Join"] = Line_Join.main
main2 ["Line_Width"] = Line_Width.main
main2 ["Linear_Gradient"] = Linear_Gradient.main
main2 ["Load_Image_Data_URL"] = Load_Image_Data_URL.main
main2 ["Load_Image_Data_URL_2"] = Load_Image_Data_URL_2.main
main2 ["Path"] = Path.main
main2 ["Pattern"] = Pattern.main
main2 ["Quadratic_Curve"] = Quadratic_Curve.main
main2 ["Radial_Gradient"] = Radial_Gradient.main
main2 ["Rectangle"] = Rectangle.main
main2 ["Red_Line"] = Red_Line.main
main2 ["Rotating_Square"] = Rotating_Square.main
main2 ["Rounded_Corners"] = Rounded_Corners.main
main2 ["Semicircle"] = Semicircle.main
main2 ["Shadow"] = Shadow.main
main2 ["Square"] = Square.main
main2 ["Text_Align"] = Text_Align.main
main2 ["Text_Baseline"] = Text_Baseline.main
main2 ["Text_Color"] = Text_Color.main
main2 ["Text_Metrics"] = Text_Metrics.main
main2 ["Text_Stroke"] = Text_Stroke.main
main2 ["Text_Wrap"] = Text_Wrap.main
main2 ["Tic_Tac_Toe"] = Tic_Tac_Toe.main
main2 ["Translate_Transform"] = Translate_Transform.main
main2 ["Scale_Transform"] = Scale_Transform.main
main2 ["Rotate_Transform"] = Rotate_Transform.main
main2 ["Custom_Transform"] = Custom_Transform.main

main2 ["clean"] = do
        _ <- createProcess $ shell "rm blank-canvas.wiki/images/*.png blank-canvas.wiki/images/*.gif blank-canvas.wiki/examples/*.hs"
        return ()

main2 args = shakeArgs shakeOptions $ do

    if null args then do
            want ["blank-canvas.wiki/images/" ++ nm ++ ".gif" | nm <- movies ]
            want ["blank-canvas.wiki/images/" ++ nm ++ ".png" | nm <- examples ++ tutorial]
            want ["blank-canvas.wiki/examples/" ++ nm ++ ".hs" | nm <- movies ++ examples ++ tutorial]
            want ["blank-canvas.wiki/" ++ toMinus nm ++ ".md" | nm <- movies ++ examples ++ tutorial]
    else return ()

    ["blank-canvas.wiki/images/*.png", "blank-canvas.wiki/images/*.gif"] |%> \out -> do
        let nm = takeBaseName out

        liftIO $ print (out,nm)

        liftIO $ removeFiles ("blank-canvas.wiki/tmp") ["*.png"]

        need [ "blank-canvas.wiki/" ++ toMinus nm ++ ".md" ]
        let haskell_file = nm ++ ".hs"
	let haskell_path = wiki_suite ++ "/" ++ haskell_file
        need [ haskell_path, "blank-canvas.wiki/examples/" ++ haskell_file ]
        liftIO $ print nm

        txt <- Shake.readFile' $ haskell_path

        let (w,h) = head $
              [ case words ln of
                 [_,_,_,n] -> read n
                 _         -> (512,384)
              | ln <- lines txt
              , "import" `L.isPrefixOf` ln && "Wiki" `L.isInfixOf` ln
              ] ++ [(512,384) :: (Int, Int)]


        sequence_ [
             do (_,_,_,ghc) <- liftIO $
                              createProcess (proc "stack" ["exec","wiki-suite",nm])

                 -- wait a second, for things to start
                liftIO $ threadDelay (1 * 1000 * 1000)
                _ <-liftIO $ openBrowser $ "http://localhost:3000/?height=" ++ show (h) ++ "&width=" ++ show (w) ++ hd
                 -- wait for haskell program to stop
                liftIO $ waitForProcess ghc | hd <- [("")] ++ if nm == "Text_Wrap" then [("&hd")] else [] ]
        return ()


    "blank-canvas.wiki/examples/*.hs" %> \ out -> do
        liftIO $ print "*hs"
        liftIO $ print out
        let haskell_file = takeFileName out

        liftIO $ print "before read file"
        txt <- Shake.readFile' $ wiki_suite ++ "/" ++ haskell_file
        liftIO $ print "after read file"

        let new = reverse
                $ dropWhile (all isSpace)
                $ reverse
                [ if "module" `L.isPrefixOf` ln
                  then "module Main where"
                  else ln
                | ln <- lines txt
                , not ("wiki $" `L.isInfixOf` ln)         -- remove the wiki stuff
                , not ("import" `L.isPrefixOf` ln && "Wiki" `L.isInfixOf` ln)
                ]

        writeFileChanged out (unlines $ map (untabify 0) new)

    "blank-canvas.wiki/*.md" %> \ out -> do
        b <- Shake.doesFileExist out
--        liftIO $ print b
        txts <- liftIO $ if b then do
                        h <- openFile out ReadMode
                        let loop = do
                             b' <- hIsEOF h
                             if b'
                             then return []
                             else do
                                ln <- hGetLine h
                                lns <- loop
                                return (ln : lns)
                        txts <- loop
                        hClose h
                        return txts
                else return []
--        liftIO $ print txts

        let p = not . (code_header `L.isPrefixOf`)
        let textToKeep = takeWhile p txts

        let haskell_file = map (\ c -> if c == '-' then '_' else c)
                         $ replaceExtension (takeFileName out) ".hs"


        liftIO $ print haskell_file
        txt <- Shake.readFile' $ "blank-canvas.wiki/examples/" ++ haskell_file

        let new = unlines $
                       [ t | t <- textToKeep
                       ] ++
                       [code_header] ++
                       lines txt ++
                       [code_footer]

--        liftIO $ putStrLn new

        writeFileChanged out new


-- to clean: rm images/*png images/*gif examples/*hs
-- */

movies :: [String]
movies = ["Rotating_Square","Tic_Tac_Toe","Bounce","Key_Read","Square"]

examples :: [String]
examples = ["Red_Line","Favicon"]
        ++ ["Color_Square"]


tutorial :: [String]
tutorial = ["Line", "Line_Width", "Line_Color", "Line_Cap","Miter_Limit"]
        ++ ["Arc","Quadratic_Curve","Bezier_Curve"]
        ++ ["Path","Line_Join","Rounded_Corners","Is_Point_In_Path"]
        ++ ["Custom_Shape","Rectangle","Circle","Semicircle"]
        ++ ["Color_Fill","Linear_Gradient","Radial_Gradient","Pattern"]
        ++ ["Draw_Image","Image_Size","Image_Crop","Image_Loader", "Draw_Canvas", "Draw_Device"]
        ++ ["Font_Size_and_Style","Text_Color","Text_Stroke","Text_Align","Text_Baseline","Text_Metrics","Text_Wrap"]
        ++ ["Translate_Transform","Scale_Transform","Rotate_Transform","Custom_Transform"]
        ++ ["Shadow","Global_Alpha","Clipping_Region","Global_Composite_Operations"]
        ++ ["Grayscale","Get_Image_Data_URL","Load_Image_Data_URL"]
        ++ ["Load_Image_Data_URL_2"]

wiki_dir :: String
wiki_dir = "."

toMinus :: String -> String
toMinus = map (\ c -> if c == '_' then '-' else c)


untabify :: Int -> String -> String
untabify _ [] = []
untabify n (c:cs) | c == '\t' = let t = 8 - n `mod` 8 in take t (cycle " ") ++ untabify (n + t) cs
                  | otherwise = c : untabify (n + 1) cs

code_header :: String
code_header = "````Haskell"

code_footer :: String
code_footer = "````"

wiki_suite :: String
wiki_suite = "wiki-suite"
