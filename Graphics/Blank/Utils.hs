{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.Utils where

import Data.ByteString.Base64  -- Not sure why to use this, vs this *.URL version. This one works, though.
import qualified Data.ByteString as B
import Data.Monoid
import Data.Text(Text)
import Data.Text.Encoding (decodeUtf8)

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

-- data:[<MIME-type>][;charset=<encoding>][;base64],<data>

-- | Read a file, and generate a data URL.
-- >  url <-  readDataURL "image/png" "image/foo.png"

readDataURL :: Text -> FilePath -> IO Text
readDataURL mime_type filePath = do
	    dat <- B.readFile filePath
	    return $ "data:" <> mime_type <> ";base64," <> decodeUtf8 (encode dat)

--         <link rel="icon" type="image/png" href="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAABxUlEQVRYR+1WPU/CUBQ9r8ENIiNs1dlEGJmssxL5Hy46GN2Uf4KJBhMHSOQH6OYmJA5Otk7qJCQshvqur6YWXmnLayGwtGuTe887H/dehhV/bMX9kQJIGfAYeEfW0ICdSVNy2JdFvdmHljkCNPL+cd6DVW0vwsAegC8g/42cJYqujwvTfQHDXWx02sKuB1JDzsqw9rrzgpA88IlcTTyzJRflxwX9qgFtTQZH6MLcLy8UgFPsA1lBLZt8bZ8wKhf12xI05gNHdbxWL+YBMZWCZUsRGMNlShE6B+JKQTdnbXDtSUkOojqrPP9JFwogiRTUPK2BsfOZIFQAOEWSSEGPWyKabDsShCqAJKmg65MGkNGjWbAtVnlxYj17GwZ4YSBiWQqOpagoBhS1Di2McuEs/IzelABESsAyptA7P/VSd0BFSqEigWtCUzTwmhDooYihETiaJSRUj0yFCoCY1A/kHeJKEZaKWQBiUU/UA9lG0l0RNorVqf/fivpdLcmumAIQj3rfMkqwttXXsd/1DvVmtSR5T2/l40rhP0jiU+/PYUwppJNM1DLkenZjfJJN/OHiGIk6yTY74oQjeUaE3A3pVZwykDLwC4/VJDBDudEqAAAAAElFTkSuQmCC"/>
