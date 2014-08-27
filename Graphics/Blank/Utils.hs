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
--   in Javascript. Example:
--
-- > grd # addColorStop(0, "#8ED6FF");
--
--   This can be seen as equivalent of @grd.addColorStop(0, "#8ED6FF")@.
(#) :: a -> (a -> Canvas b) -> Canvas b
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
