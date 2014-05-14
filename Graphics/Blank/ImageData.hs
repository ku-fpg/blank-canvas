module Graphics.Blank.ImageData where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8)

-- | 'ImageData' lives on the server.
data ImageData = ImageData !Int !Int !(Vector RGBA) deriving (Show, Eq, Ord)

-- TODO: at some point, change Vector to an *unboxed* Vector of 32bit values.

-- | RGBA is byte-level representations of the standard colors, Red: Green, Blue, Alpha.
data RGBA = RGBA !Word8 !Word8 !Word8 !Word8 deriving (Show, Eq, Ord)

