{-# LANGUAGE FlexibleInstances, OverlappingInstances, OverloadedStrings #-}

module Graphics.Blank.JavaScript where

import           Control.Applicative

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Default.Class
import           Data.Ix
import           Data.List
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder hiding (fromString)
import           Data.Text.Lazy.Builder.Int
import           Data.Text.Lazy.Builder.RealFloat
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word8)

import           Text.ParserCombinators.ReadP (skipSpaces, string)
import           Text.ParserCombinators.ReadPrec
import           Text.Read
import qualified Text.Show.Text as T
import           Text.Show.Text hiding (Show)

-------------------------------------------------------------

-- TODO: close off
class Image a where
  jsImage :: a -> Builder
  width  :: Num b => a -> b
  height :: Num b => a -> b

instance Image CanvasImage where
  jsImage = jsCanvasImage
  width  (CanvasImage _ w _) = fromIntegral w
  height (CanvasImage _ _ h) = fromIntegral h

-- The Image of a canvas is the the canvas context, not the DOM entry, so
-- you need to indirect back to the DOM here.
instance Image CanvasContext where
  jsImage = (<> ".canvas") . jsCanvasContext
  width  (CanvasContext _ w _) = fromIntegral w
  height (CanvasContext _ _ h) = fromIntegral h

-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

-- TODO: close off
class Style a where
  jsStyle :: a -> Builder

instance Style Text                 where { jsStyle = jsText }
instance Style CanvasGradient       where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern        where { jsStyle = jsCanvasPattern }
instance Style (Colour Double)      where { jsStyle = jsColour }
instance Style (AlphaColour Double) where { jsStyle = jsAlphaColour }

class Style a => CanvasColor a where
  jsCanvasColor :: a -> Builder
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
  showsPrec k = showsPrec k . showbPrec k

instance T.Show RepeatDirection where
  showb Repeat   = "repeat"
  showb RepeatX  = "repeat-x"
  showb RepeatY  = "repeat-y"
  showb NoRepeat = "no-repeat"

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
  showsPrec k = showsPrec k . showbPrec k

instance T.Show LineEndCap where
  showb ButtCap   = "butt"
  showb RoundCap  = "round"
  showb SquareCap = "square"

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
  showsPrec k = showsPrec k . showbPrec k

instance T.Show LineJoinCorner where
  showb BevelCorner = "bevel"
  showb RoundCorner = "round"
  showb MiterCorner = "miter"

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
  showsPrec k = showsPrec k . showbPrec k

instance T.Show TextAnchorAlignment where
  showb StartAnchor  = "start"
  showb EndAnchor    = "end"
  showb CenterAnchor = "center"
  showb LeftAnchor   = "left"
  showb RightAnchor  = "right"

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
  showsPrec k = showsPrec k . showbPrec k

instance T.Show TextBaselineAlignment where
  showb TopBaseline         = "top"
  showb HangingBaseline     = "hanging"
  showb MiddleBaseline      = "middle"
  showb AlphabeticBaseline  = "alphabetic"
  showb IdeographicBaseline = "ideographic"
  showb BottomBaseline      = "bottom"

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
  showbJS :: a -> Builder

instance JSArg (AlphaColour Double) where
  showbJS aCol
      | a >= 1    = jsColour rgbCol
      | a <= 0    = jsLiteralBuilder "rgba(0,0,0,0)"
      | otherwise = jsLiteralBuilder $ "rgba("
          <> showb r    <> singleton ','
          <> showb g    <> singleton ','
          <> showb b    <> singleton ','
          <> jsDouble a <> singleton ')'
    where
      a         = alphaChannel aCol
      rgbCol    = darken (recip a) $ aCol `over` black
      RGB r g b = toSRGB24 rgbCol

jsAlphaColour :: AlphaColour Double -> Builder
jsAlphaColour = showbJS

instance JSArg Bool where
  showbJS True  = "true"
  showbJS False = "false"

jsBool :: Bool -> Builder
jsBool = showbJS

instance JSArg CanvasContext where
  showbJS (CanvasContext n _ _) = "canvasbuffers[" <> showb n <> singleton ']'

jsCanvasContext :: CanvasContext -> Builder
jsCanvasContext = showbJS

instance JSArg CanvasImage where
  showbJS (CanvasImage n _ _) = "images[" <> showb n <> singleton ']'

jsCanvasImage :: CanvasImage -> Builder
jsCanvasImage = showbJS

instance JSArg CanvasGradient where
  showbJS (CanvasGradient n) = "gradient_" <> showb n

jsCanvasGradient :: CanvasGradient -> Builder
jsCanvasGradient = showbJS

instance JSArg CanvasPattern where
  showbJS (CanvasPattern n) = "patterns[" <> showb n <> singleton ']'

jsCanvasPattern :: CanvasPattern -> Builder
jsCanvasPattern = showbJS

instance JSArg (Colour Double) where
  showbJS = jsLiteralBuilder . fromString . sRGB24show

jsColour :: Colour Double -> Builder
jsColour = showbJS

instance JSArg Double where
  showbJS = formatRealFloat Fixed (Just 3)

jsDouble :: Double -> Builder
jsDouble = showbJS

instance JSArg ImageData where
  showbJS (ImageData w h d) = "ImageData(" <> showb w <> singleton ',' <> showb h <> ",[" <> vs <> "])"
     where
          vs = jsList showb $ V.toList d

jsImageData :: ImageData -> Builder
jsImageData = showbJS

instance JSArg Int where
  showbJS = showb

jsInt :: Int -> Builder
jsInt = showb

instance JSArg LineEndCap where
  showbJS = jsLiteralBuilder . showb

jsLineEndCap :: LineEndCap -> Builder
jsLineEndCap = showbJS

instance JSArg LineJoinCorner where
  showbJS = jsLiteralBuilder . showb

jsLineJoinCorner :: LineJoinCorner -> Builder
jsLineJoinCorner = showbJS

jsList :: (a -> Builder) -> [a] -> Builder
jsList js = mconcat . intersperse "," . map js

instance JSArg RepeatDirection where
  showbJS = jsLiteralBuilder . showb

jsRepeatDirection :: RepeatDirection -> Builder
jsRepeatDirection = showbJS

instance JSArg Text where
  showbJS = jsLiteralBuilder . fromText

jsText :: Text -> Builder
jsText = showbJS

instance JSArg TextAnchorAlignment where
  showbJS = jsLiteralBuilder . showb

jsTextAnchorAlignment :: TextAnchorAlignment -> Builder
jsTextAnchorAlignment = showbJS

instance JSArg TextBaselineAlignment where
  showbJS = jsLiteralBuilder . showb

jsTextBaselineAlignment :: TextBaselineAlignment -> Builder
jsTextBaselineAlignment = showbJS

-- The following was from our Sunroof compiler.
-- -------------------------------------------------------------
-- String Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Convert a Builder to a representation as a JS string literal.
jsLiteralBuilder :: Builder -> Builder
jsLiteralBuilder = jsQuoteBuilder . jsEscapeBuilder

-- | Add quotes to a Builder.
jsQuoteBuilder :: Builder -> Builder
jsQuoteBuilder s = singleton '\"' <> s <> singleton '\"'

-- | Transform a character to a lazy Text that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> TL.Text
jsUnicodeChar c =
  let hex = toLazyText . hexadecimal $ ord c
  in "\\u" <> TL.replicate (4 - TL.length hex) (TL.singleton '0') <> hex

-- | Correctly replace a Builder's characters by the JS escape sequences.
jsEscapeBuilder :: Builder -> Builder
jsEscapeBuilder = fromLazyText . TL.concatMap jsEscapeChar . toLazyText

-- | Correctly replace Haskell characters by the JS escape sequences.
jsEscapeChar :: Char -> TL.Text
jsEscapeChar '\\' = "\\\\"
-- Special control sequences.
jsEscapeChar '\0' = jsUnicodeChar '\0' -- Ambigous with numbers
jsEscapeChar '\a' = jsUnicodeChar '\a' -- Non JS
jsEscapeChar '\b' = "\\b"
jsEscapeChar '\f' = "\\f"
jsEscapeChar '\n' = "\\n"
jsEscapeChar '\r' = "\\r"
jsEscapeChar '\t' = "\\t"
jsEscapeChar '\v' = "\\v"
jsEscapeChar '\"' = "\\\""
jsEscapeChar '\'' = "\\'"
-- Non-control ASCII characters can remain as they are.
jsEscapeChar c' | not (isControl c') && isAscii c' = TL.singleton c'
-- All other non ASCII signs are escaped to unicode.
jsEscapeChar c' = jsUnicodeChar c'
