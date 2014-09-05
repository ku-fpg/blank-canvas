{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.JavaScript where

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.List
import           Data.Text (Text, unpack)
import           Data.Word (Word8)

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

import           Numeric

-------------------------------------------------------------

-- TODO: close off 
class Image a where
  jsImage :: a -> String
  width  :: Num b => a -> b
  height :: Num b => a -> b

instance Image CanvasImage where 
  jsImage = jsCanvasImage
  width  (CanvasImage _ w _) = fromIntegral w 
  height (CanvasImage _ _ h) = fromIntegral h
  
-- The Image of a canvas is the the canvas context, not the DOM entry, so
-- you need to indirect back to the DOM here.
instance Image CanvasContext where
  jsImage = (++ ".canvas") . jsCanvasContext
  width  (CanvasContext _ w _) = fromIntegral w 
  height (CanvasContext _ _ h) = fromIntegral h

-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

-- TODO: close off 
class Style a where
  jsStyle :: a -> String

instance Style Text                where { jsStyle = jsText }
instance Style CanvasGradient      where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern       where { jsStyle = jsCanvasPattern }
instance Style (Colour Float)      where { jsStyle = jsColour }
instance Style (AlphaColour Float) where { jsStyle = jsAlphaColour }

class Style a => CanvasColor a

instance CanvasColor Text
instance CanvasColor (Colour Float)
instance CanvasColor (AlphaColour Float)

-------------------------------------------------------------

-- | A handle to an offscreen canvas. CanvasContext can not be destroyed.
data CanvasContext = CanvasContext Int Int Int
 deriving (Show,Eq,Ord)                

-- | A handle to the Image. CanvasImages can not be destroyed.
data CanvasImage = CanvasImage Int Int Int deriving (Show,Eq,Ord)

-- | A handle to the CanvasGradient. CanvasGradients can not be destroyed.
newtype CanvasGradient = CanvasGradient Int deriving (Show,Eq,Ord)

-- | A handle to the CanvasPattern. CanvasPatterns can not be destroyed.
newtype CanvasPattern = CanvasPattern Int deriving (Show,Eq,Ord)

-------------------------------------------------------------

-- | 'ImageData' is a transliteration of the JavaScript ImageData,
--   There are two 'Int's, and one (unboxed) 'Vector' of 'Word8's.
--  width, height, data can be projected from 'ImageData',
--  'Vector.length' can be used to find the length.
-- 
--   Note: 'ImageData' lives on the server, not the client.

data ImageData = ImageData !Int !Int !(Vector Word8) deriving (Show, Eq, Ord)

-------------------------------------------------------------

class JSArg a where
  showJS :: a -> String

instance JSArg (AlphaColour Float) where
    showJS aCol
        | a >= 1    = jsColour rgbCol
        | a <= 0    = jsLiteralString "rgba(0,0,0,0)"
        | otherwise = jsLiteralString $ "rgba("
            ++ show r    ++ ","
            ++ show g    ++ ","
            ++ show b    ++ ","
            ++ jsFloat a ++ ")"
      where
        a         = alphaChannel aCol
        rgbCol    = darken (recip a) $ aCol `over` black
        RGB r g b = toSRGB24 rgbCol

jsAlphaColour :: AlphaColour Float -> String
jsAlphaColour = showJS

instance JSArg Float where
  showJS a = showFFloat (Just 3) a ""        

jsFloat :: Float -> String
jsFloat = showJS 

instance JSArg Int where
  showJS a = show a

instance JSArg CanvasContext where
  showJS (CanvasContext n _ _) = "canvasbuffers[" ++ show n ++ "]"

jsCanvasContext :: CanvasContext -> String
jsCanvasContext = showJS 

instance JSArg CanvasImage where
  showJS (CanvasImage n _ _) = "images" ++ show n

jsCanvasImage :: CanvasImage -> String
jsCanvasImage = showJS 

instance JSArg CanvasGradient where
  showJS (CanvasGradient n) = "gradients" ++ show n

jsCanvasGradient :: CanvasGradient -> String
jsCanvasGradient = showJS 

instance JSArg CanvasPattern where
  showJS (CanvasPattern n) = "patterns[" ++ show n ++ "]"

jsCanvasPattern :: CanvasPattern -> String
jsCanvasPattern = showJS

instance JSArg (Colour Float) where
    showJS = jsLiteralString . sRGB24show

jsColour :: Colour Float -> String
jsColour = showJS

instance JSArg ImageData where
  showJS (ImageData w h d) = "ImageData(" ++ show w ++ "," ++ show h ++ ",[" ++ vs ++ "])"
     where
          vs = jsList show $ V.toList d

jsImageData :: ImageData -> String
jsImageData = showJS

instance JSArg Bool where
  showJS True  = "true"
  showJS False = "false"

jsBool :: Bool -> String
jsBool = showJS

instance JSArg Text where 
  showJS = jsLiteralString . unpack

jsText :: Text -> String
jsText = showJS

jsList :: (a -> String) -> [a] -> String
jsList js = concat . intersperse "," . map js 


-- The following was from our Sunroof compiler.
-- -------------------------------------------------------------
-- String Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Transform a Haskell string into a string representing a JS string literal.
jsLiteralString :: String -> String
jsLiteralString = jsQuoteString . jsEscapeString

-- | Add quotes to a string.
jsQuoteString :: String -> String
jsQuoteString s = "\"" ++ s ++ "\""

-- | Transform a character to a string that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> String
jsUnicodeChar c =
  let hex = showHex (ord c) ""
  in ('\\':'u': replicate (4 - length hex) '0') ++ hex

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeString :: String -> String
jsEscapeString [] = []
jsEscapeString (c:cs) = case c of
  -- Backslash has to remain backslash in JS.
  '\\' -> '\\' : '\\' : jsEscapeString cs
  -- Special control sequences.
  '\0' -> jsUnicodeChar '\0' ++ jsEscapeString cs -- Ambigous with numbers
  '\a' -> jsUnicodeChar '\a' ++ jsEscapeString cs -- Non JS
  '\b' -> '\\' : 'b' : jsEscapeString cs
  '\f' -> '\\' : 'f' : jsEscapeString cs
  '\n' -> '\\' : 'n' : jsEscapeString cs
  '\r' -> '\\' : 'r' : jsEscapeString cs
  '\t' -> '\\' : 't' : jsEscapeString cs
  '\v' -> '\\' : 'v' : jsEscapeString cs
  '\"' -> '\\' : '\"' : jsEscapeString cs
  '\'' -> '\\' : '\'' : jsEscapeString cs
  -- Non-control ASCII characters can remain as they are.
  c' | not (isControl c') && isAscii c' -> c' : jsEscapeString cs
  -- All other non ASCII signs are escaped to unicode.
  c' -> jsUnicodeChar c' ++ jsEscapeString cs 
