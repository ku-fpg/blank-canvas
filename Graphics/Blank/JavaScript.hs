{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Graphics.Blank.JavaScript where

import           Data.Char (isControl, isAscii, ord)
import           Data.Colour
import           Data.Colour.SRGB
import           Data.Default.Class
import           Data.Ix
import           Data.Monoid ((<>))
import           Data.List
import           Data.String
import           Data.Text.Lazy (Text, fromStrict)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as ST
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, toList)
import           Data.Word (Word8)

import           Graphics.Blank.Parser
import           Graphics.Blank.Instr
import qualified Graphics.Blank.Instr as I

import           Prelude.Compat

import           Text.ParserCombinators.ReadP (choice, skipSpaces)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..), parens, readListPrecDefault)

import           TextShow hiding (toLazyText)
import           TextShow.TH (deriveTextShow)

-------------------------------------------------------------

-- | A handle to an offscreen canvas. 'CanvasContext' cannot be destroyed.
data CanvasContext = CanvasContext Int Int Int deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasContext)
instance InstrShow CanvasContext

-- | A handle to a canvas image. 'CanvasImage's cannot be destroyed.
data CanvasImage = CanvasImage Int Int Int     deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasImage)
instance InstrShow CanvasImage

-- | A handle to the a canvas gradient. 'CanvasGradient's cannot be destroyed.
newtype CanvasGradient = CanvasGradient Int    deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasGradient)

-- | A handle to a canvas pattern. 'CanvasPattern's cannot be destroyed.
newtype CanvasPattern = CanvasPattern Int      deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasPattern)
instance InstrShow CanvasPattern

-- | A handle to a canvas audio. 'CanvasAudio's cannot be destroyed.
data CanvasAudio = CanvasAudio !Int !Double    deriving (Eq, Ord, Show)
$(deriveTextShow ''CanvasAudio)
instance InstrShow CanvasAudio

-- module local names as not in scope until after all the splices
instance InstrShow CanvasGradient where showi = jsCanvasGradient

-------------------------------------------------------------

-- | 'ImageData' is a transliteration of JavaScript's
-- @<https://developer.mozilla.org/en-US/docs/Web/API/ImageData ImageData>@.
-- 'ImageData' consists of two 'Int's and one (unboxed) 'Vector' of 'Word8's.
-- @width@, @height@, and @data@ can be projected from 'ImageData',
-- 'Vector.length' can be used to find the @data@ length.
--
-- Note: 'ImageData' lives on the server, not the client.

data ImageData = ImageData !Int !Int !(Vector Word8) deriving (Eq, Ord, Show)

-- Defined manually to avoid an orphan T.Show (Vector a) instance
instance InstrShow ImageData where
    showi = showiPrec 0
    showiPrec p (ImageData w h d) = surroundIf (p > 10) "(" ")" <>
        "ImageData " <> showiPrec 11 w <> I.singleton ' '
                     <> showiPrec 11 h <> I.singleton ' '
                     <> showiUnaryWith tshowiPrec "fromList" 11 (toList d)

-------------------------------------------------------------

-- | Class for JavaScript objects that represent images (including the canvas itself).
class Image a where
    jsImage :: a -> Instr
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
    jsAudio       :: a -> Instr
    durationAudio :: Fractional b => a -> b
    indexAudio    :: a -> Int -- the index to access the audio in the sounds array 


instance Audio CanvasAudio where         
  jsAudio                         = jsCanvasAudio
  durationAudio (CanvasAudio _ d) = realToFrac d
  indexAudio    (CanvasAudio n _) = n

-- instance Element Video  -- Not supported

-----------------------------------------------------------------------------

-- | A data type that can represent a style. That is, something with one or more
-- colors.
class Style a where
    -- | Convert a value into a JavaScript string representing a style value.
    jsStyle :: a -> Instr

instance Style Text                 where { jsStyle = jsText }
instance Style CanvasGradient       where { jsStyle = jsCanvasGradient }
instance Style CanvasPattern        where { jsStyle = jsCanvasPattern }
instance Style (Colour Double)      where { jsStyle = jsColour }
instance Style (AlphaColour Double) where { jsStyle = jsAlphaColour }

-- | A 'Style' containing exactly one color.
class Style a => CanvasColor a

jsCanvasColor :: CanvasColor color => color -> Instr
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

instance Show RepeatDirection where

    showsPrec p = showsPrec p . FromTextShow

instance TextShow RepeatDirection where
    showb Repeat   = "repeat"
    showb RepeatX  = "repeat-x"
    showb RepeatY  = "repeat-y"
    showb NoRepeat = "no-repeat"

instance InstrShow RepeatDirection

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
    round_ = RoundCap

instance Show LineEndCap where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow LineEndCap where
    showb ButtCap   = "butt"
    showb RoundCap  = "round"
    showb SquareCap = "square"

instance InstrShow LineEndCap

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
    round_ = RoundCorner

instance Show LineJoinCorner where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow LineJoinCorner where
    showb BevelCorner = "bevel"
    showb RoundCorner = "round"
    showb MiterCorner = "miter"

instance InstrShow LineJoinCorner

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

instance Show TextAnchorAlignment where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow TextAnchorAlignment where
    showb StartAnchor  = "start"
    showb EndAnchor    = "end"
    showb CenterAnchor = "center"
    showb LeftAnchor   = "left"
    showb RightAnchor  = "right"

instance InstrShow TextAnchorAlignment

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

instance Show TextBaselineAlignment where
    showsPrec p = showsPrec p . FromTextShow

instance TextShow TextBaselineAlignment where
    showb TopBaseline         = "top"
    showb HangingBaseline     = "hanging"
    showb MiddleBaseline      = "middle"
    showb AlphabeticBaseline  = "alphabetic"
    showb IdeographicBaseline = "ideographic"
    showb BottomBaseline      = "bottom"

instance InstrShow TextBaselineAlignment

-- | Class for @round@ CSS property values.
class RoundProperty a where
    -- | Shorthand for 'RoundCap' or 'RoundCorner', with an underscore to
    -- distinguish it from 'round'.
    round_ :: a

-------------------------------------------------------------

-- | Class for Haskell data types which represent JavaScript data.
class JSArg a where
    -- | Display a value as JavaScript data.
    showiJS :: a -> Instr

instance JSArg (AlphaColour Double) where
  showiJS = jsAlphaColour

jsAlphaColour :: AlphaColour Double -> Instr
jsAlphaColour aCol
    | a >= 1    = jsColour rgbCol
    | a <= 0    = jsLiteralBuilder "rgba(0,0,0,0)"
    | otherwise = jsLiteralBuilder $ "rgba("
        <> showi r    <> I.singleton ','
        <> showi g    <> I.singleton ','
        <> showi b    <> I.singleton ','
        <> jsDouble a <> I.singleton ')'
  where
    a         = alphaChannel aCol
    rgbCol    = darken (recip a) $ aCol `over` black
    RGB r g b = toSRGB24 rgbCol

instance JSArg Bool where
    showiJS = jsBool

jsBool :: Bool -> Instr
jsBool True  = "true"
jsBool False = "false"

instance JSArg CanvasAudio where
  showiJS = jsCanvasAudio

jsCanvasAudio :: CanvasAudio -> Instr
jsCanvasAudio (CanvasAudio n _ ) = "sounds[" <> showi n <> I.singleton ']'

jsIndexAudio :: CanvasAudio -> Instr
jsIndexAudio (CanvasAudio n _) = showi n

instance JSArg CanvasContext where
    showiJS = jsCanvasContext

jsCanvasContext :: CanvasContext -> Instr
jsCanvasContext (CanvasContext n _ _) = "canvasbuffers[" <> showi n <> I.singleton ']'

instance JSArg CanvasImage where
    showiJS = jsCanvasImage

jsCanvasImage :: CanvasImage -> Instr
jsCanvasImage (CanvasImage n _ _) = "images[" <> showi n <> I.singleton ']'

instance JSArg CanvasGradient where
    showiJS = jsCanvasGradient

jsCanvasGradient :: CanvasGradient -> Instr
jsCanvasGradient (CanvasGradient n) = "gradient_" <> showi n

instance JSArg CanvasPattern where
    showiJS = jsCanvasPattern

jsCanvasPattern :: CanvasPattern -> Instr
jsCanvasPattern (CanvasPattern n) = "pattern_" <> showi n

instance JSArg (Colour Double) where
    showiJS = jsColour

jsColour :: Colour Double -> Instr
jsColour = jsLiteralBuilder . sRGB24showi

-- | Convert a colour in hexadecimal 'Instr' form, e.g. \"#00aaff\"
sRGB24showi :: (Floating b, RealFrac b) => Colour b -> Instr
sRGB24showi c =
    I.singleton '#' <> showiHex2 r' <> showiHex2 g' <> showiHex2 b'
  where
    RGB r' g' b' = toSRGB24 c
    showiHex2 x | x <= 0xf = I.singleton '0' <> showiHex x
                | otherwise = showiHex x

instance JSArg Double where
    showiJS = jsDouble

jsDouble :: Double -> Instr
jsDouble = showi

instance JSArg ImageData where
    showiJS = jsImageData

jsImageData :: ImageData -> Instr
jsImageData (ImageData w h d) = "ImageData(" <> showi w
    <> I.singleton ',' <> showi h
    <> ",[" <> vs <> "])"
  where
    vs = jsList showi $ V.toList d

instance JSArg Int where
    showiJS = jsInt

jsInt :: Int -> Instr
jsInt = showi

instance JSArg LineEndCap where
    showiJS = jsLineEndCap

jsLineEndCap :: LineEndCap -> Instr
jsLineEndCap = jsLiteralBuilder . showi

instance JSArg LineJoinCorner where
    showiJS = jsLineJoinCorner

jsLineJoinCorner :: LineJoinCorner -> Instr
jsLineJoinCorner = jsLiteralBuilder . showi

jsList :: (a -> Instr) -> [a] -> Instr
jsList js = mconcat . intersperse "," . map js

instance JSArg RepeatDirection where
    showiJS = jsRepeatDirection

jsRepeatDirection :: RepeatDirection -> Instr
jsRepeatDirection = jsLiteralBuilder . showi

instance JSArg Text where
    showiJS = jsText

instance JSArg ST.Text where
    showiJS = jsText . fromStrict

jsText :: TL.Text -> Instr
jsText = jsLiteralBuilder . I.fromText

instance JSArg TextAnchorAlignment where
    showiJS = jsTextAnchorAlignment

jsTextAnchorAlignment :: TextAnchorAlignment -> Instr
jsTextAnchorAlignment = jsLiteralBuilder . showi

instance JSArg TextBaselineAlignment where
    showiJS = jsTextBaselineAlignment

jsTextBaselineAlignment :: TextBaselineAlignment -> Instr
jsTextBaselineAlignment = jsLiteralBuilder . showi

-- The following was adapted from our Sunroof compiler.
-- -------------------------------------------------------------
-- Instr Conversion Utilities: Haskell -> JS
-- -------------------------------------------------------------

-- | Convert a 'Instr' to a representation as a JS string literal.
jsLiteralBuilder :: Instr -> Instr
jsLiteralBuilder = jsQuoteBuilder . jsEscapeBuilder

-- | Add quotes to a 'Instr'.
jsQuoteBuilder :: Instr -> Instr
jsQuoteBuilder b = I.singleton '"' <> b <> I.singleton '"'

-- | Transform a character to a lazy 'TL.Text' that represents its JS
--   unicode escape sequence.
jsUnicodeChar :: Char -> TL.Text
jsUnicodeChar c =
    let hex = toLazyText . showiHex $ ord c
    in "\\u" <> TL.replicate (4 - TL.length hex) (TL.singleton '0') <> hex

-- | Correctly replace a `Instr'`s characters by the JS escape sequences.
jsEscapeBuilder :: Instr -> Instr
jsEscapeBuilder = fromBuilder . fromLazyText . TL.concatMap jsEscapeChar . toLazyText

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
