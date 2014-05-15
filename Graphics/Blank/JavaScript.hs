{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.JavaScript where

import Data.List
import Data.Word (Word8)
import Numeric

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)

-------------------------------------------------------------

-- | A handle to an offscreen canvas. CanvasContext can not be destroyed.
data CanvasContext = CanvasContext Int 
                   | TopCanvas
 deriving (Show,Eq,Ord)                

top :: CanvasContext
top = TopCanvas

-- | A handle to the Image. CanvasImages can not be destroyed.
newtype CanvasImage = CanvasImage Int deriving (Show,Eq,Ord)

-- | A handle to the CanvasGradient. CanvasGradients can not be destroyed.
newtype CanvasGradient = CanvasGradient Int deriving (Show,Eq,Ord)

-- | A handle to the CanvasPattern. CanvasPatterns can not be destroyed.
newtype CanvasPattern = CanvasPattern Int deriving (Show,Eq,Ord)

-------------------------------------------------------------

-- | 'ImageData' lives on the server, not the client.
data ImageData = ImageData !Int !Int !(Vector Word8) deriving (Show, Eq, Ord)

-------------------------------------------------------------

class JSArg a where
  showJS :: a -> String

instance JSArg Float where
  showJS a = showFFloat (Just 3) a ""        

instance JSArg CanvasContext where
  showJS (CanvasContext n) = "canvasbuffers[" ++ show n ++ "]"
  showJS (TopCanvas)      = "c"

instance JSArg CanvasImage where
  showJS (CanvasImage n) = "images[" ++ show n ++ "]"

instance JSArg CanvasGradient where
  showJS (CanvasGradient n) = "gradients[" ++ show n ++ "]"

instance JSArg CanvasPattern where
  showJS (CanvasPattern n) = "patterns[" ++ show n ++ "]"

instance JSArg ImageData where
  showJS (ImageData w h d) = "ImageData(" ++ show w ++ "," ++ show h ++ ",[])"

instance JSArg Bool where
  showJS True  = "true"
  showJS False = "false"

instance JSArg [Char] where 
  showJS str = show str

instance JSArg a => JSArg [a] where 
  showJS = concat . intersperse "," . map showJS 
