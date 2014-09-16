{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Graphics.Blank.JavaScript where

import           Control.Applicative

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Default.Class
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
data RepeatDirection = Repeat   -- ^ The pattern repeats both horizontally
                                --   and vertically.
                     | RepeatX  -- ^ The pattern repeats only horizontally.
                     | RepeatY  -- ^ The pattern repeats only vertically.
                     | NoRepeat -- ^ The pattern displays only once and
                                --   does not repeat.
  deriving Eq

instance Default RepeatDirection where
  def = Repeat

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

-- | The style of the caps on the endpoints of a line.
data LineEndCap = ButtCap   -- ^ Flat edges
                | RoundCap  -- ^ Semicircular end caps
                | SquareCap -- ^ Square end caps
  deriving Eq

instance Default LineEndCap where
  def = ButtCap

instance IsString LineEndCap where
  fromString = read

instance Read LineEndCap where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "butt"   -> return ButtCap  
          "round"  -> return RoundCap 
          "square" -> return SquareCap
          _        -> pfail

instance Show LineEndCap where
  showsPrec _ le = showString $ case le of
      ButtCap   -> "butt"
      RoundCap  -> "round"
      SquareCap -> "square"

-- | The style of corner that is created when two lines join.
data LineJoinCorner = BevelCorner -- ^ A filled triangle with a beveled edge
                                  --   connects two lines.
                    | RoundCorner -- ^ A filled arc connects two lines.
                    | MiterCorner -- ^ A filled triangle with a sharp edge
                                  --   connects two lines.
  deriving Eq

instance Default LineJoinCorner where
  def = MiterCorner

instance IsString LineJoinCorner where
  fromString = read

instance Read LineJoinCorner where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "bevel" -> return BevelCorner
          "round" -> return RoundCorner
          "miter" -> return MiterCorner
          _       -> pfail

instance Show LineJoinCorner where
  showsPrec _ corner = showString $ case corner of
      BevelCorner -> "bevel"
      RoundCorner -> "round"
      MiterCorner -> "miter"

-- | The anchor point for text in the current 'DeviceContext'.
data TextAnchorAlignment = StartAnchor  -- ^ The text is anchored at either its left edge
                                        --   (if the canvas is left-to-right) or its right
                                        --   edge (if the canvas is right-to-left).
                         | EndAnchor    -- ^ The text is anchored at either its right edge
                                        --   (if the canvas is left-to-right) or its left
                                        --   edge (if the canvas is right-to-left).
                         | CenterAnchor -- ^ The text is anchored in its center.
                         | LeftAnchor   -- ^ The text is anchored at its left edge.
                         | RightAnchor  -- ^ the text is anchored at its right edge.
  deriving Eq

instance Default TextAnchorAlignment where
  def = StartAnchor

instance IsString TextAnchorAlignment where
  fromString = read

instance Read TextAnchorAlignment where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "start"  -> return StartAnchor
          "end"    -> return EndAnchor
          "center" -> return CenterAnchor
          "left"   -> return LeftAnchor
          "right"  -> return RightAnchor
          _        -> pfail

instance Show TextAnchorAlignment where
  showsPrec _ align = showString $ case align of
      StartAnchor  -> "start"
      EndAnchor    -> "end"
      CenterAnchor -> "center"
      LeftAnchor   -> "left"
      RightAnchor  -> "right"

-- | The baseline alignment used when drawing text in the current 'DeviceContext'.
--   The baselines are ordered from highest ('Top') to lowest ('Bottom').
data TextBaselineAlignment = TopBaseline
                           | HangingBaseline
                           | MiddleBaseline
                           | AlphabeticBaseline
                           | IdeographicBaseline
                           | BottomBaseline
  deriving (Bounded, Eq, Ix, Ord)

instance Default TextBaselineAlignment where
  def = AlphabeticBaseline

instance IsString TextBaselineAlignment where
  fromString = read

instance Read TextBaselineAlignment where
  readPrec = parens $ do
      Ident s <- lexP
      case s of
          "top"         -> return TopBaseline
          "hanging"     -> return HangingBaseline
          "middle"      -> return MiddleBaseline
          "alphabetic"  -> return AlphabeticBaseline
          "ideographic" -> return IdeographicBaseline
          "bottom"      -> return BottomBaseline
          _             -> pfail

instance Show TextBaselineAlignment where
  showsPrec _ bl = showString $ case bl of
      TopBaseline         -> "top"
      HangingBaseline     -> "hanging"
      MiddleBaseline      -> "middle"
      AlphabeticBaseline  -> "alphabetic"
      IdeographicBaseline -> "ideographic"
      BottomBaseline      -> "bottom"

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

instance JSArg LineEndCap where
  showJS = jsLiteralString . show

jsLineEndCap :: LineEndCap -> String
jsLineEndCap = showJS

instance JSArg LineJoinCorner where
  showJS = jsLiteralString . show

jsLineJoinCorner :: LineJoinCorner -> String
jsLineJoinCorner = showJS

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

instance JSArg TextAnchorAlignment where
  showJS = jsLiteralString . show

jsTextAnchorAlignment :: TextAnchorAlignment -> String
jsTextAnchorAlignment = showJS

instance JSArg TextBaselineAlignment where
  showJS = jsLiteralString . show

jsTextBaselineAlignment :: TextBaselineAlignment -> String
jsTextBaselineAlignment = showJS

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
