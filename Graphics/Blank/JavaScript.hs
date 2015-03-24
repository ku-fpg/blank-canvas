{-# LANGUAGE CPP, FlexibleInstances, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Blank.JavaScript where

import           Control.Applicative

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Default.Class
import           Data.Ix
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid (mconcat)
#endif
import           Data.Monoid ((<>))
import           Data.List
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B (singleton)
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox, toList)
import           Data.Word (Word8)

import           Graphics.Blank.Parser

import           Prelude hiding (Show, round)

import           Text.ParserCombinators.ReadP (choice, skipSpaces)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..), parens, readListPrecDefault)
import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)
import           Text.Show.Text.Data.Floating (showbFFloat)
import           Text.Show.Text.Data.Integral (showbHex)
import           Text.Show.Text.TH (deriveShow)

-------------------------------------------------------------

-- | A handle to an offscreen canvas. CanvasContext can not be destroyed.
data CanvasContext = CanvasContext Int Int Int deriving (Eq, Ord, S.Show)
$(deriveShow ''CanvasContext)

-- | A handle to the Image. CanvasImages can not be destroyed.
data CanvasImage = CanvasImage Int Int Int     deriving (Eq, Ord, S.Show)
$(deriveShow ''CanvasImage)

-- | A handle to the CanvasGradient. CanvasGradients can not be destroyed.
newtype CanvasGradient = CanvasGradient Int    deriving (Eq, Ord, S.Show)
$(deriveShow ''CanvasGradient)

-- | A handle to the CanvasPattern. CanvasPatterns can not be destroyed.
newtype CanvasPattern = CanvasPattern Int      deriving (Eq, Ord, S.Show)
$(deriveShow ''CanvasPattern)

-------------------------------------------------------------

-- | 'ImageData' is a transliteration of the JavaScript ImageData,
--   There are two 'Int's, and one (unboxed) 'Vector' of 'Word8's.
--  width, height, data can be projected from 'ImageData',
--  'Vector.length' can be used to find the length.
--
--   Note: 'ImageData' lives on the server, not the client.

data ImageData = ImageData !Int !Int !(Vector Word8) deriving (Eq, Ord, S.Show)
$(deriveShow ''ImageData)

data AudioInfo = AudioInfo !Int !Double deriving (Eq, Ord, S.Show)
$(deriveShow ''AudioInfo)

-- Borrowed from @text-show-instances@
instance (T.Show a, Unbox a) => T.Show (Vector a) where
    showbPrec p = showbUnary "fromList" p . toList

-------------------------------------------------------------

-- | Class for JavaScript objects that represent images (including the canvas itself).
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

class Audio a where
    jsAudio    :: a -> Builder
    duration   :: Fractional b => a -> b

instance Audio AudioInfo where         
  jsAudio                     = jsAudioInfo
  duration  (AudioInfo _ d)   = realToFrac d

-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

-- | A data type that can represent a style. That is, something with one or more
-- colors.
class Style a where
    -- | Convert a value into a JavaScript string representing a style value.
    jsStyle :: a -> Builder

instance Style Text                 where { jsStyle = jsText }
instance Style CanvasGradient       where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern        where { jsStyle = jsCanvasPattern }
instance Style (Colour Double)      where { jsStyle = jsColour }
instance Style (AlphaColour Double) where { jsStyle = jsAlphaColour }

-- | A 'Style' containing exactly one color.
class Style a => CanvasColor a

jsCanvasColor :: CanvasColor color => color -> Builder
jsCanvasColor = jsStyle

instance CanvasColor Text
instance CanvasColor (Colour Double)
instance CanvasColor (AlphaColour Double)

-------------------------------------------------------------

-- | The direction in which a 'CanvasPattern' repeats.
data RepeatDirection = Repeat   -- ^ The pattern repeats both horizontally
                                --   and vertically (default).
                     | RepeatX  -- ^ The pattern repeats only horizontally.
                     | RepeatY  -- ^ The pattern repeats only vertically.
                     | NoRepeat -- ^ The pattern displays only once and
                                --   does not repeat.
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'Repeat', with an underscore to distinguish it from 'repeat'.
repeat_ :: RepeatDirection
repeat_ = Repeat

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

instance S.Show RepeatDirection where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show RepeatDirection where
    showb Repeat   = "repeat"
    showb RepeatX  = "repeat-x"
    showb RepeatY  = "repeat-y"
    showb NoRepeat = "no-repeat"

-- | The style of the caps on the endpoints of a line.
data LineEndCap = ButtCap   -- ^ Flat edges (default).
                | RoundCap  -- ^ Semicircular end caps
                | SquareCap -- ^ Square end caps
  deriving (Bounded, Enum, Eq, Ix, Ord)

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
    round = RoundCap

instance S.Show LineEndCap where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show LineEndCap where
    showb ButtCap   = "butt"
    showb RoundCap  = "round"
    showb SquareCap = "square"

-- | The style of corner that is created when two lines join.
data LineJoinCorner = BevelCorner -- ^ A filled triangle with a beveled edge
                                  --   connects two lines.
                    | RoundCorner -- ^ A filled arc connects two lines.
                    | MiterCorner -- ^ A filled triangle with a sharp edge
                                  --   connects two lines (default).
  deriving (Bounded, Enum, Eq, Ix, Ord)

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
    round = RoundCorner

instance S.Show LineJoinCorner where
    showsPrec p = showsPrec p . FromTextShow

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
  deriving (Bounded, Enum, Eq, Ix, Ord)

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

instance S.Show TextAnchorAlignment where
    showsPrec p = showsPrec p . FromTextShow

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
  deriving (Bounded, Enum, Eq, Ix, Ord)

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

instance S.Show TextBaselineAlignment where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show TextBaselineAlignment where
    showb TopBaseline         = "top"
    showb HangingBaseline     = "hanging"
    showb MiddleBaseline      = "middle"
    showb AlphabeticBaseline  = "alphabetic"
    showb IdeographicBaseline = "ideographic"
    showb BottomBaseline      = "bottom"

-- | The type of compositing operation to apply when drawing new shapes. See
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/globalCompositeOperation here>
-- for an illustrated guide to the compositing modes.
data CompositeMode = SourceOverMode      -- ^ Draws new shapes on top of existing shapes.
                   | SourceAtopMode      -- ^ Draws new shapes only on top of where the new shapes and the existing shapes overlap.
                   | SourceInMode        -- ^ Draws new shapes only on top of where the new shapes and the existing shapes overlap.
                                         --   Everything else is made transparent.
                   | SourceOutMode       -- ^ Draws new shapes only where the new shapes and the existing shapes don't overlap.
                                         --   Everything else is made transparent.
                   | DestinationOverMode -- ^ Draws new shapes behind existing shapes.
                   | DestinationAtopMode -- ^ Draws new shapes, and draws existing shapes only on top of where the new shapes
                                         --   and the existing shapes overlap.
                   | DestinationInMode   -- ^ Draws existing shapes only on top of where the new shapes and the existing shapes overlap.
                                        --    Everything else is made transparent.
                   | DestinationOutMode  -- ^ Draws existing shapes only where the new shapes and the existing shapes don't overlap.
                                         --   Everything else is made transparent.
                   | LighterMode         -- ^ Where the new shapes and the existing shapes overlap is determined by adding color values.
                   | CopyMode            -- ^ Draws only the new images, ignoring the existing images.
                   | XorMode             -- ^ Where the new shapes and the existing shapes overlap is made transparent.
  deriving (Bounded, Enum, Eq, Ix, Ord)

-- | Shorthand for 'SourceOverMode'.
sourceOver :: CompositeMode
sourceOver = SourceOverMode

-- | Shorthand for 'SourceAtopMode'.
sourceAtop :: CompositeMode
sourceAtop = SourceAtopMode

-- | Shorthand for 'SourceIntMode'.
sourceIn :: CompositeMode
sourceIn = SourceInMode

-- | Shorthand for 'SourceOutMode'.
sourceOut :: CompositeMode
sourceOut = SourceOutMode

-- | Shorthand for 'DestinationOverMode'.
destinationOver :: CompositeMode
destinationOver = DestinationOverMode

-- | Shorthand for 'DestinationAtopMode'.
destinationAtop :: CompositeMode
destinationAtop = DestinationAtopMode

-- | Shorthand for 'DestinationInMode'.
destinationIn :: CompositeMode
destinationIn = DestinationInMode

-- | Shorthand for 'DestinationOutMode'.
destinationOut :: CompositeMode
destinationOut = DestinationOutMode

-- | Shorthand for 'LighterMode'.
lighter :: CompositeMode
lighter = LighterMode

-- | Shorthand for 'XorMode'.
xor :: CompositeMode
xor = XorMode

instance CopyProperty CompositeMode where
    copy = CopyMode

instance Default CompositeMode where
    def = SourceOverMode

instance IsString CompositeMode where
    fromString = read

instance Read CompositeMode where
    readPrec = parens . lift $ do
        skipSpaces
        choice
            [ SourceOverMode      <$ stringCI "source-over"
            , SourceAtopMode      <$ stringCI "source-atop"
            , SourceInMode        <$ stringCI "source-in"
            , SourceOutMode       <$ stringCI "source-out"
            , DestinationOverMode <$ stringCI "destination-over"
            , DestinationAtopMode <$ stringCI "destination-atop"
            , DestinationInMode   <$ stringCI "destination-in"
            , DestinationOutMode  <$ stringCI "destination-out"
            , LighterMode         <$ stringCI "lighter"
            , CopyMode            <$ stringCI "copy"
            , XorMode             <$ stringCI "xor"
            ]
    readListPrec = readListPrecDefault

instance S.Show CompositeMode where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show CompositeMode where
    showb SourceOverMode      = "source-over"
    showb SourceAtopMode      = "source-atop"
    showb SourceInMode        = "source-in"
    showb SourceOutMode       = "source-out"
    showb DestinationOverMode = "destination-over"
    showb DestinationAtopMode = "destination-atop"
    showb DestinationInMode   = "destination-in"
    showb DestinationOutMode  = "destination-out"
    showb LighterMode         = "lighter"
    showb CopyMode            = "copy"
    showb XorMode             = "xor"

-- | Class for @round@ CSS property values.
class RoundProperty a where
    -- | Shorthand for 'RoundCap' or 'RoundCorner'.
    round :: a

-- | Class for @copy@ CSS property values.
class CopyProperty a where
    -- | Shorthand for 'CopyMode' or 'CopyCursor'.
    copy :: a

-------------------------------------------------------------

class JSArg a where
    showbJS :: a -> Builder

instance JSArg (AlphaColour Double) where
  showbJS = jsAlphaColour

jsAlphaColour :: AlphaColour Double -> Builder
jsAlphaColour aCol
    | a >= 1    = jsColour rgbCol
    | a <= 0    = jsLiteralBuilder "rgba(0,0,0,0)"
    | otherwise = jsLiteralBuilder $ "rgba("
        <> showb r    <> B.singleton ','
        <> showb g    <> B.singleton ','
        <> showb b    <> B.singleton ','
        <> jsDouble a <> B.singleton ')'
  where
    a         = alphaChannel aCol
    rgbCol    = darken (recip a) $ aCol `over` black
    RGB r g b = toSRGB24 rgbCol

instance JSArg AudioInfo where
  showbJS = jsAudioInfo

jsAudioInfo :: AudioInfo -> Builder
jsAudioInfo (AudioInfo n _ ) = "sounds[" <> showb n <> B.singleton ']'

instance JSArg Bool where
    showbJS = jsBool

jsBool :: Bool -> Builder
jsBool True  = "true"
jsBool False = "false"

instance JSArg CanvasContext where
    showbJS = jsCanvasContext

jsCanvasContext :: CanvasContext -> Builder
jsCanvasContext (CanvasContext n _ _) = "canvasbuffers[" <> showb n <> B.singleton ']'

instance JSArg CanvasImage where
    showbJS = jsCanvasImage

jsCanvasImage :: CanvasImage -> Builder
jsCanvasImage (CanvasImage n _ _) = "images[" <> showb n <> B.singleton ']'

instance JSArg CanvasGradient where
    showbJS = jsCanvasGradient

jsCanvasGradient :: CanvasGradient -> Builder
jsCanvasGradient (CanvasGradient n) = "gradient_" <> showb n

instance JSArg CanvasPattern where
    showbJS = jsCanvasPattern

jsCanvasPattern :: CanvasPattern -> Builder
jsCanvasPattern (CanvasPattern n) = "patterns[" <> showb n <> B.singleton ']'

instance JSArg (Colour Double) where
    showbJS = jsColour

jsColour :: Colour Double -> Builder
jsColour = jsLiteralBuilder . sRGB24showb

-- | Convert a colour in hexadecimal 'Builder' form, e.g. \"#00aaff\"
sRGB24showb :: (Floating b, RealFrac b) => Colour b -> Builder
sRGB24showb c =
    B.singleton '#' <> showbHex2 r' <> showbHex2 g' <> showbHex2 b'
  where
    RGB r' g' b' = toSRGB24 c
    showbHex2 x | x <= 0xf = B.singleton '0' <> showbHex x
                | otherwise = showbHex x

instance JSArg CompositeMode where
    showbJS = jsCompositeMode

jsCompositeMode :: CompositeMode -> Builder
jsCompositeMode = jsLiteralBuilder . showb

instance JSArg Double where
    showbJS = jsDouble

jsDouble :: Double -> Builder
jsDouble = showbFFloat $ Just 3

instance JSArg ImageData where
    showbJS = jsImageData

jsImageData :: ImageData -> Builder
jsImageData (ImageData w h d) = "ImageData(" <> showb w
    <> B.singleton ',' <> showb h
    <> ",[" <> vs <> "])"
  where
    vs = jsList showb $ V.toList d

instance JSArg Int where
    showbJS = jsInt

jsInt :: Int -> Builder
jsInt = showb

instance JSArg LineEndCap where
    showbJS = jsLineEndCap

jsLineEndCap :: LineEndCap -> Builder
jsLineEndCap = jsLiteralBuilder . showb

instance JSArg LineJoinCorner where
    showbJS = jsLineJoinCorner

jsLineJoinCorner :: LineJoinCorner -> Builder
jsLineJoinCorner = jsLiteralBuilder . showb

jsList :: (a -> Builder) -> [a] -> Builder
jsList js = mconcat . intersperse "," . map js

instance JSArg RepeatDirection where
    showbJS = jsRepeatDirection

jsRepeatDirection :: RepeatDirection -> Builder
jsRepeatDirection = jsLiteralBuilder . showb

instance JSArg Text where
    showbJS = jsText

jsText :: Text -> Builder
jsText = jsLiteralBuilder . fromText

instance JSArg TextAnchorAlignment where
    showbJS = jsTextAnchorAlignment

jsTextAnchorAlignment :: TextAnchorAlignment -> Builder
jsTextAnchorAlignment = jsLiteralBuilder . showb

instance JSArg TextBaselineAlignment where
    showbJS = jsTextBaselineAlignment

jsTextBaselineAlignment :: TextBaselineAlignment -> Builder
jsTextBaselineAlignment = jsLiteralBuilder . showb

-- The following was adapted from our Sunroof compiler.
-- -------------------------------------------------------------
-- Builder Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Convert a 'Builder' to a representation as a JS string literal.
jsLiteralBuilder :: Builder -> Builder
jsLiteralBuilder = jsQuoteBuilder . jsEscapeBuilder

-- | Add quotes to a 'Builder'.
jsQuoteBuilder :: Builder -> Builder
jsQuoteBuilder b = B.singleton '"' <> b <> B.singleton '"'

-- | Transform a character to a lazy 'TL.Text' that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> TL.Text
jsUnicodeChar c =
    let hex = toLazyText . showbHex $ ord c
    in "\\u" <> TL.replicate (4 - TL.length hex) (TL.singleton '0') <> hex

-- | Correctly replace a `Builder'`s characters by the JS escape sequences.
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
