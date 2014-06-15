{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.JavaScript where

import Data.List
import Data.Word (Word8)
import Numeric
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

-------------------------------------------------------------

-- TODO: close off 
class Image a where
  jsImage :: a -> String
        
instance Image CanvasImage   where { jsImage = jsCanvasImage }
-- The Image of a canvas is the DOM entry, not the canvas context, so
-- you need to indirect back to the canvas DOM here.
instance Image CanvasContext where { jsImage = (++ ".canvas") . jsCanvasContext }
-- instance Element Video  -- Not supported


-----------------------------------------------------------------------------

-- TODO: close off 
class Style a where
  jsStyle :: a -> String

instance Style Text           where { jsStyle = jsText }
instance Style CanvasGradient where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern  where { jsStyle = jsCanvasPattern }

-------------------------------------------------------------

-- | A handle to an offscreen canvas. CanvasContext can not be destroyed.
data CanvasContext = CanvasContext Int 
                   | TopCanvas
 deriving (Show,Eq,Ord)                

-- | 'top' is the main Canvas.
top :: CanvasContext
top = TopCanvas

-- | A handle to the Image. CanvasImages can not be destroyed.
newtype CanvasImage = CanvasImage Int deriving (Show,Eq,Ord)

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

instance JSArg Float where
  showJS a = showFFloat (Just 3) a ""        

jsFloat = showJS :: Float -> String

instance JSArg Int where
  showJS a = show a

jsInt = showJS :: Int -> String

instance JSArg CanvasContext where
  showJS (CanvasContext n) = "canvasbuffers[" ++ show n ++ "]"
  showJS (TopCanvas)      = "c"

jsCanvasContext = showJS :: CanvasContext -> String

instance JSArg CanvasImage where
  showJS (CanvasImage n) = "images[" ++ show n ++ "]"

jsCanvasImage = showJS :: CanvasImage -> String

instance JSArg CanvasGradient where
  showJS (CanvasGradient n) = "gradients[" ++ show n ++ "]"

jsCanvasGradient = showJS :: CanvasGradient -> String

instance JSArg CanvasPattern where
  showJS (CanvasPattern n) = "patterns[" ++ show n ++ "]"

jsCanvasPattern = showJS :: CanvasPattern -> String

instance JSArg ImageData where
  showJS (ImageData w h d) = "ImageData(" ++ show w ++ "," ++ show h ++ ",[" ++ vs ++ "])"
     where
          vs = jsList (\ x -> "0x" ++ showHex x "") $ V.toList d

jsImageData = showJS :: ImageData -> String

instance JSArg Bool where
  showJS True  = "true"
  showJS False = "false"

jsBool = showJS :: Bool -> String

instance JSArg Text where 
  showJS str = show str

jsText :: Text -> String
jsText = showJS

instance JSArg a => JSArg [a] where 
  showJS = jsList showJS

jsList :: (a -> String) -> [a] -> String
jsList js = concat . intersperse "," . map js 
