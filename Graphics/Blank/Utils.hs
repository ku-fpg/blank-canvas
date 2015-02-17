{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.Utils where

import Data.ByteString.Base64  -- Not sure why to use this, vs this *.URL version. This one works, though.
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Graphics.Blank.Canvas
import Graphics.Blank.Generated
import Graphics.Blank.JavaScript

-- | Clear the screen. Restores the default transformation matrix.
clearCanvas :: Canvas ()
clearCanvas = do
  setTransform (1, 0, 0, 1, 0, 0)
  me <- myCanvasContext
  clearRect (0,0,width me,height me)

-- | Wrap a canvas computation in 'save' / 'restore'.
saveRestore :: Canvas a -> Canvas a
saveRestore m = do
    save ()
    r <- m
    restore ()
    return r

infixr 0 #

-- | The @#@-operator is the Haskell analog to the @.@-operator
--   in JavaScript. Example:
-- 
-- > grd # addColorStop(0, "#8ED6FF");
-- 
--   This can be seen as equivalent of @grd.addColorStop(0, "#8ED6FF")@.
(#) :: a -> (a -> b) -> b
(#) obj act = act obj

-- | Read a file, and generate a data URL.
--
-- >  url <- readDataURL "image/png" "image/foo.png"
--
readDataURL :: Text -> FilePath -> IO Text
readDataURL mime_type filePath = do
    dat <- B.readFile filePath
    return $ "data:" <> mime_type <> ";base64," <> decodeUtf8 (encode dat)

-- | Find the mime type for a data URL.
--
-- > > dataURLMimeType "data:image/png;base64,iVBORw..."
-- > "image/png"
--
dataURLMimeType :: Text -> Text
dataURLMimeType txt
    | dat /= "data" = error "dataURLMimeType: no 'data:'"
    | not (Text.null rest0) && not (Text.null rest2) = mime_type
    | otherwise = error "dataURLMimeType: bad parse"
 where
   (dat,rest0)       = Text.span (/= ':') txt
   Just (_,rest1)    = Text.uncons rest0
   (mime_type,rest2) = Text.span (/= ';') rest1

-- | Write a data URL to a given file.

writeDataURL :: FilePath -> Text -> IO ()
writeDataURL fileName
             = B.writeFile fileName
             . decodeLenient
             . encodeUtf8
             . Text.tail
             . Text.dropWhile (/= ',')

-- | Draws an image onto the canvas at the given x- and y-coordinates.
drawImageAt :: Image image => (image, Double, Double) -> Canvas ()
drawImageAt (img, dx, dy) = Method $ DrawImage (img, [dx, dy])

-- | Acts like 'drawImageAt', but with two extra 'Double' arguments. The third and fourth
--   'Double's specify the width and height of the image, respectively.
drawImageSize :: Image image => (image, Double, Double, Double, Double) -> Canvas ()
drawImageSize (img, dx, dy, dw, dh) = Method $ DrawImage (img, [dx, dy, dw, dh])

-- | Acts like 'drawImageSize', but with four extra 'Double' arguments before the arguments
--   of 'drawImageSize'. The first and second 'Double's specify the x- and y-coordinates at
--   which the image begins to crop. The third and fourth 'Double's specify the width and
--   height of the cropped image.
-- 
-- @
-- 'drawImageCrop' img 0 0 dw dh dx dy dw dh = 'drawImageSize' = dx dy dw dh
-- @
drawImageCrop :: Image image => (image, Double, Double, Double, Double, Double, Double, Double, Double) -> Canvas ()
drawImageCrop (img, sx, sy, sw, sh, dx, dy, dw, dh)
  = Method $ DrawImage (img, [sx, sy, sw, sh, dx, dy, dw, dh])

-- | Writes 'ImageData' to the canvas at the given x- and y-coordinates.
putImageDataAt :: (ImageData, Double, Double) -> Canvas ()
putImageDataAt (imgData, dx, dy) = Method $ PutImageData (imgData, [dx, dy])

-- | Acts like 'putImageDataAt', but with four extra 'Double' arguments that specify
--   which region of the 'ImageData' (the dirty rectangle) should be drawn. The third
--   and fourth 'Double's specify the dirty rectangle's x- and y- coordinates, and the
--   fifth and sixth 'Double's specify the dirty rectangle's width and height.
--   
-- @
-- 'putImageDataDirty' imgData dx dy 0 0 w h = 'putImageDataAt' imgData dx dy
--   where (w, h) = case imgData of ImageData w' h' _ -> (w', h')
-- @
putImageDataDirty :: (ImageData, Double, Double, Double, Double, Double, Double) -> Canvas ()
putImageDataDirty (imgData, dx, dy, dirtyX, dirtyY, dirtyWidth, dirtyHeight)
  = Method $ PutImageData (imgData, [dx, dy, dirtyX, dirtyY, dirtyWidth, dirtyHeight])
