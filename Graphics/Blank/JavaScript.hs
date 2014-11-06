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
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word8)

import           Graphics.Blank.Parser
import           Graphics.Blank.Types.Font

import           Numeric

import           Text.ParserCombinators.ReadP (choice, skipSpaces)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..), parens, readListPrecDefault)

-----------------------------------------------------------------------------

class CanvasFont a where
    jsCanvasFont :: a -> String

instance CanvasFont Text where
    jsCanvasFont = jsText

instance CanvasFont Font where
    jsCanvasFont = jsFont

-----------------------------------------------------------------------------

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

instance Style Text                 where { jsStyle = jsText }
instance Style CanvasGradient       where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern        where { jsStyle = jsCanvasPattern }
instance Style (Colour Double)      where { jsStyle = jsColour }
instance Style (AlphaColour Double) where { jsStyle = jsAlphaColour }

class Style a => CanvasColor a

jsCanvasColor :: CanvasColor color => color -> String
jsCanvasColor = jsStyle

instance CanvasColor Text
instance CanvasColor (Colour Double)
instance CanvasColor (AlphaColour Double)

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
                                --   and vertically (default).
                     | RepeatX  -- ^ The pattern repeats only horizontally.
                     | RepeatY  -- ^ The pattern repeats only vertically.
                     | NoRepeat -- ^ The pattern displays only once and
                                --   does not repeat.
  deriving Eq

-- | Shorthand for 'Repeat', with a quote to distinguish it from 'repeat'.
repeat' :: RepeatDirection
repeat' = Repeat

-- | Shorthand for 'RepeatX'.
repeatX :: RepeatDirection
repeatX = RepeatX

-- | Shorthand for 'RepeatY'.
repeatY :: RepeatDirection
repeatY = RepeatY

-- | Shorthand for 'NoRepeat'.
noRepeat :: RepeatDirection
noRepeat = NoRepeat

instance Default RepeatDirection where
  def = Repeat

instance IsString RepeatDirection where
  fromString = read

instance Read RepeatDirection where
  readPrec = parens . lift $ do
      skipSpaces
      choice
          [ Repeat   <$ stringCI "repeat"
          , RepeatX  <$ stringCI "repeat-x"
          , RepeatY  <$ stringCI "repeat-y"
          , NoRepeat <$ stringCI "no-repeat"
          ]
  readListPrec = readListPrecDefault

instance Show RepeatDirection where
  showsPrec _ rd = showString $ case rd of
      Repeat   -> "repeat"
      RepeatX  -> "repeat-x"
      RepeatY  -> "repeat-y"
      NoRepeat -> "no-repeat"

-- | The style of the caps on the endpoints of a line.
data LineEndCap = ButtCap   -- ^ Flat edges (default).
                | RoundCap  -- ^ Semicircular end caps
                | SquareCap -- ^ Square end caps
  deriving Eq

-- | Shorthand for 'ButtCap'.
butt :: LineEndCap
butt = ButtCap

-- | Shorthand for 'SquareCap'.
square :: LineEndCap
square = SquareCap

instance Default LineEndCap where
  def = ButtCap

instance IsString LineEndCap where
  fromString = read

instance Read LineEndCap where
  readPrec = parens . lift $ do
      skipSpaces
      choice
          [ ButtCap   <$ stringCI "butt"
          , RoundCap  <$ stringCI "round"
          , SquareCap <$ stringCI "square"
          ]
  readListPrec = readListPrecDefault

instance RoundProperty LineEndCap where
  round' = RoundCap

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
                                  --   connects two lines (default).
  deriving Eq

-- | Shorthand for 'BevelCorner'.
bevel :: LineJoinCorner
bevel = BevelCorner

-- | Shorthand for 'MiterCorner'.
miter :: LineJoinCorner
miter = MiterCorner

instance Default LineJoinCorner where
  def = MiterCorner

instance IsString LineJoinCorner where
  fromString = read

instance Read LineJoinCorner where
  readPrec = parens . lift $ do
      skipSpaces
      choice
          [ BevelCorner <$ stringCI "bevel"
          , RoundCorner <$ stringCI "round"
          , MiterCorner <$ stringCI "miter"
          ]
  readListPrec = readListPrecDefault

instance RoundProperty LineJoinCorner where
  round' = RoundCorner

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

-- | Shorthand for 'StartAnchor'.
start :: TextAnchorAlignment
start = StartAnchor

-- | Shorthand for 'EndAnchor'.
end :: TextAnchorAlignment
end = EndAnchor

-- | Shorthand for 'CenterAnchor'.
center :: TextAnchorAlignment
center = CenterAnchor

-- | Shorthand for 'LeftAnchor'.
left :: TextAnchorAlignment
left = LeftAnchor

-- | Shorthand for 'RightAnchor'.
right :: TextAnchorAlignment
right = RightAnchor

instance Default TextAnchorAlignment where
  def = StartAnchor

instance IsString TextAnchorAlignment where
  fromString = read

instance Read TextAnchorAlignment where
  readPrec = parens . lift $ do
      skipSpaces
      choice
          [ StartAnchor  <$ stringCI "start"
          , EndAnchor    <$ stringCI "end"
          , CenterAnchor <$ stringCI "center"
          , LeftAnchor   <$ stringCI "left"
          , RightAnchor  <$ stringCI "right"
          ]
  readListPrec = readListPrecDefault

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

-- | Shorthand for 'TopBaseline'.
top :: TextBaselineAlignment
top = TopBaseline

-- | Shorthand for 'HangingBaseline'.
hanging :: TextBaselineAlignment
hanging = HangingBaseline

-- | Shorthand for 'MiddleBaseline'.
middle :: TextBaselineAlignment
middle = MiddleBaseline

-- | Shorthand for 'AlphabeticBaseline'.
alphabetic :: TextBaselineAlignment
alphabetic = AlphabeticBaseline

-- | Shorthand for 'IdeographicBaseline'.
ideographic :: TextBaselineAlignment
ideographic = IdeographicBaseline

-- | Shorthand for 'BottomBaseline'.
bottom :: TextBaselineAlignment
bottom = BottomBaseline

instance Default TextBaselineAlignment where
  def = AlphabeticBaseline

instance IsString TextBaselineAlignment where
  fromString = read

instance Read TextBaselineAlignment where
  readPrec = parens . lift $ do
      skipSpaces
      choice
          [ TopBaseline         <$ stringCI "top"
          , HangingBaseline     <$ stringCI "hanging"
          , MiddleBaseline      <$ stringCI "middle"
          , AlphabeticBaseline  <$ stringCI "alphabetic"
          , IdeographicBaseline <$ stringCI "ideographic"
          , BottomBaseline      <$ stringCI "bottom"
          ]
  readListPrec = readListPrecDefault

instance Show TextBaselineAlignment where
  showsPrec _ bl = showString $ case bl of
      TopBaseline         -> "top"
      HangingBaseline     -> "hanging"
      MiddleBaseline      -> "middle"
      AlphabeticBaseline  -> "alphabetic"
      IdeographicBaseline -> "ideographic"
      BottomBaseline      -> "bottom"

class RoundProperty a where
  -- | Shorthand for 'RoundCap' or 'RoundCorner', with a quote to distinguish it from 'round'.
  round' :: a

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

instance JSArg (AlphaColour Double) where
  showJS aCol
      | a >= 1    = jsColour rgbCol
      | a <= 0    = jsLiteralString "rgba(0,0,0,0)"
      | otherwise = jsLiteralString $ "rgba("
          ++ show r     ++ ","
          ++ show g     ++ ","
          ++ show b     ++ ","
          ++ jsDouble a ++ ")"
    where
      a         = alphaChannel aCol
      rgbCol    = darken (recip a) $ aCol `over` black
      RGB r g b = toSRGB24 rgbCol

jsAlphaColour :: AlphaColour Double -> String
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
  showJS (CanvasGradient n) = "gradient_" ++ show n

jsCanvasGradient :: CanvasGradient -> String
jsCanvasGradient = showJS

instance JSArg CanvasPattern where
  showJS (CanvasPattern n) = "patterns[" ++ show n ++ "]"

jsCanvasPattern :: CanvasPattern -> String
jsCanvasPattern = showJS

instance JSArg (Colour Double) where
  showJS = jsLiteralString . sRGB24show

jsColour :: Colour Double -> String
jsColour = showJS

instance JSArg Double where
  showJS a = showFFloat (Just 3) a ""

jsDouble :: Double -> String
jsDouble = showJS

instance JSArg Font where
  showJS = jsLiteralString . show

jsFont :: Font -> String
jsFont = showJS

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
