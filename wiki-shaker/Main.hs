-- Shake Generator for wiki pages

import Development.Shake hiding (doesFileExist)
import qualified Development.Shake as Shake
import qualified Development.Shake.Command as Command
import Development.Shake.FilePath
import System.Process
import System.Directory (removeFile)
import qualified System.Directory as Dir
import System.Environment
import System.IO
import Data.List
import Control.Concurrent
import Data.Char
import System.Directory

-- to clean: rm images/*png images/*gif examples/*hs

movies = ["Rotating_Square","Tic_Tac_Toe","Bounce","Key_Read","Square"]

examples = ["Red_Line","Favicon"]

tutorial = ["Line", "Line_Width", "Line_Color", "Line_Cap","Miter_Limit"]
        ++ ["Arc","Quadratic_Curve","Bezier_Curve"]
        ++ ["Path","Line_Join","Rounded_Corners"]
        ++ ["Custom_Shape","Rectangle","Circle","Semicircle"]
	++ ["Color_Fill","Linear_Gradient","Radial_Gradient","Pattern"]
	++ ["Draw_Image","Image_Size","Image_Crop","Image_Loader"]
        ++ ["Font_Size_and_Style","Text_Color","Text_Stroke","Text_Align","Text_Baseline","Text_Metrics","Text_Wrap"]
	++ ["Translate_Transform"]
        ++ ["Shadow","Global_Alpha","Clipping_Region","Global_Composite_Operations"]
        ++ ["Get_Image_Data_URL","Load_Image_Data_URL"]

wiki_dir = "blank-canvas.wiki"

main = shakeArgs shakeOptions $ do
    want [wiki_dir ++ "/images/" ++ nm ++ ".gif" | nm <- movies]
    want [wiki_dir ++ "/images/" ++ nm ++ ".png" | nm <- examples ++ tutorial]
    want [wiki_dir ++ "/examples/" ++ nm ++ ".hs" | nm <- movies ++ examples ++ tutorial]

    [wiki_dir ++ "/images/*.png", wiki_dir ++ "/images/*.gif"] |*> \out -> do
        let nm = takeBaseName out

	liftIO $ print (out,nm)

        liftIO $ createDirectoryIfMissing False "tmp"
        liftIO $ removeFiles "tmp" [nm ++ "*.png"]
        liftIO $ createDirectoryIfMissing False "tmp"

        need [ wiki_dir ++ "/" ++ (map (\ c -> if c == '_' then '-' else c) nm) ++ ".md" ]
        let haskell_file = nm ++ ".hs"
        need [ "wiki-suite/" ++ haskell_file, wiki_dir ++ "/examples/" ++ haskell_file ]        
        liftIO $ print nm

        txt <- readFile' $ "wiki-suite/" ++ haskell_file

        let (w,h) = head $
              [ case words ln of
                 [_,_,_,n] -> read n
                 _ -> (512,384)
              | ln <- lines txt 
              , "import Wiki" `isPrefixOf` ln
              ] ++ [(512,384)]


        (_,_,_,ghc) <- liftIO $ 
                      createProcess (proc "./dist/build/wiki-suite/wiki-suite" [nm])

         -- wait a second, for things to start
        liftIO $ threadDelay (1 * 1000 * 1000)

        command_ [] "/usr/bin/open" 
                               ["-a"
                               ,"/Applications/Google Chrome.app"
                               ,"http://localhost:3000/?height=" ++ show h ++ "&width=" ++ show w]
         -- wait for haskell program to stop
        v <- liftIO $ waitForProcess ghc
        return ()

    "blank-canvas.wiki/examples/*.hs" *> \ out -> do
        liftIO $ print out
        let haskell_file = takeFileName out

        txt <- readFile' $ "wiki-suite/" ++ haskell_file

        let new = reverse
                $ dropWhile (all isSpace)
                $ reverse
                [ if "module" `isPrefixOf` ln 
		  then "module Main where"
		  else ln
                | ln <- lines txt 
                , not ("wiki $" `isInfixOf` ln)         -- remove the wiki stuff
                , not ("import Wiki" `isPrefixOf` ln)
                ]

        writeFileChanged out (unlines $ map (untabify 0) new)

    "blank-canvas.wiki/*.md" *> \ out -> do
        b <- Shake.doesFileExist out
--        liftIO $ print b
        txts <- liftIO $ if b then do
                        h <- openFile out ReadMode
                        let loop = do
                       	     b <- hIsEOF h 
                	     if b
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

        let p = not . (code_header `isPrefixOf`)
        let textToKeep = takeWhile p txts

        let haskell_file = map (\ c -> if c == '-' then '_' else c) 
	    		 $ dropDirectory1
	    		 $ replaceExtension out ".hs"

        txt <- readFile' $ wiki_dir ++ "/examples/" ++ haskell_file 

        let new = unlines $
                       [ t | t <- textToKeep 
                       ] ++
                       ["````Haskell"] ++
                       lines txt ++
                       ["````"]

--        liftIO $ putStrLn new

        writeFileChanged out new

{-
examples = -- ["Red-Line.md","Rotating-Square.md","Tic-Tac-Toe.md","Favicon.md"] ++
       {-["Line.md"
	 ,"Line-Width.md"
	 ,"Line-Color.md"
	 ,"Line-Cap.md"
	 ,"Arc.md"
	 ,"Quadratic-Curve.md"
	 ,"Bezier-Curve.md"
	 ,"Path.md"
	 ,"Line-Join.md"
	 ,"Rounded-Corners.md"
	 ,"Custom-Shape.md"
	 ,"Rectangle.md"
	 ,"Circle.md"
	 ,"Semicircle.md"
	 ,"Shape-Fill.md"
	 ,"Linear-Gradient.md"
	 ,"Radial-Gradient.md"
	 ,"Pattern.md"
	 ,"Image.md"
	 ,"Image-Size.md"
	 ,"Image-Crop.md"
	 ,"Image-Loader.md"
       -}["Text-Font-and-Size.md"
	 ,"Text-Color.md"
	 ,"Text-Stroke.md"
	 ,"Text-Align.md"
	 ,"Text-Baseline.md"
	 ,"Text-Metrics.md"
	 ,"Text-Wrap.md"
	 ,"Translate-Transform.md"
	 ,"Shadow.md"
	 ,"Global-Alpha.md"
	 ,"Clipping-Region.md"
	 ,"Global-Composite-Operations.md"
	 ,"Get-Image-Data-URL.md"
	 ]
-}
{-
main' :: IO ()
main' = do
        args <- getArgs
        main2 args


main2 :: [String] -> IO ()
main2 [] = sequence_ [ update file | file <- examples ]
main2 names = sequence_ [ update file | file <- names ]


update :: String -> IO ()
update fileName = do
        b <- doesFileExist fileName
        txts <- if b then do
                        h <- openFile fileName ReadMode
                        let loop = do
                       	     b <- hIsEOF h 
                	     if b
                	     then return []
                	     else do
                    	   	ln <- hGetLine h
                		lns <- loop
                		return (ln : lns)
                        txts <- loop 
                        hClose h
                        return txts
                else return []         
--       print txts

        let p = not . (code_header `isPrefixOf`)
        let textToKeep = takeWhile p txts

        let haskell_file = map (\ c -> if c == '-' then '_' else c) $ replaceExtension fileName ".hs"

        txt <- readFile haskell_file


        let prog = reverse
                $ dropWhile (all isSpace)
                $ reverse
                [ ln 
                | ln <- lines txt 
                , not ("wiki $" `isInfixOf` ln)         -- remove the wiki stuff
                , not ("import Wiki" `isPrefixOf` ln)
                ]
                       
        let (w,h) = head $
              [ case words ln of
                 [_,_,_,n] -> read n
                 _ -> (512,384)
              | ln <- lines txt 
              , "import Wiki" `isPrefixOf` ln
              ] ++ [(512,384)]


--       print txt
        
        let new = unlines $
                       textToKeep ++
                       ["````Haskell"] ++
                       map (untabify 0) prog  ++
                       ["````"]



        if unlines (lines new) == unlines txts then putStrLn ("Not updating: " ++ fileName) else do
               h <- openFile fileName WriteMode
               hPutStr h new
               hClose h              
               putStrLn ("Updated: " ++ fileName) 

        -- Now, run the example
        putStrLn ("Rebuilding examples for: " ++ fileName)
        (_,_,_,ghc) <- createProcess (proc "ghc" ["-ignore-dot-ghci","-x","hs"
                                                ,"-e",":set prog " ++ show haskell_file
                                                ,"-e",":main []"
                                                ,haskell_file
                                                ])
                                                

         -- wait a second, for things to start
        threadDelay (2 * 1000 * 1000)
 
        _browser <- createProcess (proc "/usr/bin/open" 
                               ["-a"
                               ,"/Applications/Google Chrome.app"
                               ,"http://localhost:3000/?height context=" ++ show h ++ "&width context=" ++ show w])
         -- wait 2 seconds, for things to start
        threadDelay (3 * 1000 * 1000)
        terminateProcess ghc

        putStrLn ("[Done]")

        -- open(location, '_self').close();
-}

untabify n [] = []
untabify n (c:cs) | c == '\t' = let t = 8 - n `mod` 8 in take t (cycle " ") ++ untabify (n + t) cs
                  | otherwise = c : untabify (n + 1) cs 

 
code_header = "````Haskell"
