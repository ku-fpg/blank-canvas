{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.JavaScript where

import           Control.Applicative

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Ix
import           Data.List
import           Data.String
import           Data.Text (Text, unpack)
import           Data.Word (Word8)

import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

import           Numeric

import           Text.ParserCombinators.ReadP (skipSpaces, string)
import           Text.ParserCombinators.ReadPrec
import           Text.Read

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

jsCanvasColor :: CanvasColor color => color -> String
jsCanvasColor = jsStyle

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

-- | The direction in which a 'CanvasPattern' repeats.
data RepeatDirection = Repeat   -- ^ The pattern repeats both horizontally and vertically.
                     | RepeatX  -- ^ The pattern repeats only horizontally.
                     | RepeatY  -- ^ The pattern repeats only vertically.
                     | NoRepeat -- ^ The pattern displays only once and does not repeat.
  deriving Eq

instance IsString RepeatDirection where
  fromString = read

instance Read RepeatDirection where
  readPrec = parens . lift $ do
      skipSpaces
      (string "repeat"          >> return Repeat)
        <|> (string "repeat-x"  >> return RepeatX)
        <|> (string "repeat-y"  >> return RepeatY)
        <|> (string "no-repeat" >> return NoRepeat)

instance Show RepeatDirection where
  showsPrec _ rd = showString $ case rd of
      Repeat   -> "repeat"
      RepeatX  -> "repeat-x"
      RepeatY  -> "repeat-y"
      NoRepeat -> "no-repeat"

-- | The style of the ends of a line.
data LineEnds = Butt       -- ^ Flat edges, no caps
              | RoundCaps  -- ^ Semicircular end caps
              | SquareCaps -- ^ Square end caps
  deriving Eq

instance IsString LineEnds where
  fromString = read

instance Read LineEnds where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "butt"   -> return Butt
          "round"  -> return RoundCaps
          "square" -> return SquareCaps
          _        -> pfail

instance Show LineEnds where
  showsPrec _ le = showString $ case le of
      Butt       -> "butt"
      RoundCaps  -> "round"
      SquareCaps -> "square"

-- | The style of corner that is created when two lines meet.
data Corner = BevelCorner -- ^ A filled triangle with a beveled edge connects two lines.
            | RoundCorner -- ^ A filled arc connects two lines.
            | MiterCorner -- ^ A filled triangle with a sharp edge connects two lines.
  deriving Eq

instance IsString Corner where
  fromString = read

instance Read Corner where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "bevel" -> return BevelCorner
          "round" -> return RoundCorner
          "miter" -> return MiterCorner
          _       -> pfail

instance Show Corner where
  showsPrec _ corner = showString $ case corner of
      BevelCorner -> "bevel"
      RoundCorner -> "round"
      MiterCorner -> "miter"

-- | The anchor point for text in the current 'DeviceContext'.
data Alignment = StartAlign  -- ^ The text is anchored at either its left edge (if the canvas is left-to-right) or its right edge (if the canvas is right-to-left).
               | EndAlign    -- ^ The text is anchored at either its right edge (if the canvas is left-to-right) or its left edge (if the canvas is right-to-left).
               | CenterAlign -- ^ The text is anchored in its center.
               | LeftAlign   -- ^ The text is anchored at its left edge.
               | RightAlign  -- ^ the text is anchored at its right edge.
  deriving Eq

instance IsString Alignment where
  fromString = read

instance Read Alignment where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "start"  -> return StartAlign
          "end"    -> return EndAlign
          "center" -> return CenterAlign
          "left"   -> return LeftAlign
          "right"  -> return RightAlign
          _        -> pfail

instance Show Alignment where
  showsPrec _ align = showString $ case align of
      StartAlign  -> "start"
      EndAlign    -> "end"
      CenterAlign -> "center"
      LeftAlign   -> "left"
      RightAlign  -> "right"

-- | The baseline alignment used when drawing text in the current 'DeviceContext'. The baselines are ordered from highest ('Top') to lowest ('Bottom').
data Baseline = Top
              | Hanging
              | Middle
              | Alphabetic
              | Ideographic
              | Bottom
  deriving (Bounded, Eq, Ix, Ord)

instance IsString Baseline where
  fromString = read

instance Read Baseline where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "top"         -> return Top
          "hanging"     -> return Hanging
          "middle"      -> return Middle
          "alphabetic"  -> return Alphabetic
          "ideographic" -> return Ideographic
          "bottom"      -> return Bottom
          _             -> pfail

instance Show Baseline where
  showsPrec _ bl = showString $ case bl of
      Top         -> "top"
      Hanging     -> "hanging"
      Middle      -> "middle"
      Alphabetic  -> "alphabetic"
      Ideographic -> "ideographic"
      Bottom      -> "bottom"

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

instance JSArg Alignment where
  showJS = jsLiteralString . show

jsAlignment :: Alignment -> String
jsAlignment = showJS

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

instance JSArg Baseline where
  showJS = jsLiteralString . show

jsBaseline :: Baseline -> String
jsBaseline = showJS

instance JSArg Bool where
  showJS True  = "true"
  showJS False = "false"

jsBool :: Bool -> String
jsBool = showJS

instance JSArg CanvasContext where
  showJS (CanvasContext n _ _) = "canvasbuffers[" ++ show n ++ "]"

jsCanvasContext :: CanvasContext -> String
jsCanvasContext = showJS

instance JSArg CanvasImage where
  showJS (CanvasImage n _ _) = "images[" ++ show n ++ "]"

jsCanvasImage :: CanvasImage -> String
jsCanvasImage = showJS

instance JSArg CanvasGradient where
  showJS (CanvasGradient n) = "gradients[" ++ show n ++ "]"

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

instance JSArg Corner where
  showJS = jsLiteralString . show

jsCorner :: Corner -> String
jsCorner = showJS

instance JSArg Float where
  showJS a = showFFloat (Just 3) a ""

jsFloat :: Float -> String
jsFloat = showJS

instance JSArg ImageData where
  showJS (ImageData w h d) = "ImageData(" ++ show w ++ "," ++ show h ++ ",[" ++ vs ++ "])"
     where
          vs = jsList show $ V.toList d

jsImageData :: ImageData -> String
jsImageData = showJS

instance JSArg Int where
  showJS a = show a

instance JSArg LineEnds where
  showJS = jsLiteralString . show

jsLineEnds :: LineEnds -> String
jsLineEnds = showJS

jsList :: (a -> String) -> [a] -> String
jsList js = concat . intersperse "," . map js

instance JSArg RepeatDirection where
  showJS = jsLiteralString . show

jsRepeatDirection :: RepeatDirection -> String
jsRepeatDirection = showJS

instance JSArg Text where
  showJS = jsLiteralString . unpack

jsText :: Text -> String
jsText = showJS

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
