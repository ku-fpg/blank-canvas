{-# LANGUAGE FlexibleInstances #-}
module Graphics.Blank.Types.Font
    ( -- * @font@
      Font(..)
    , defFont
    , caption
    , icon
    , menu
    , messageBox
    , smallCaption
    , statusBar
    -- * @font-style@
    , FontStyle(..)
    , italic
    , oblique
    -- * @font-variant@
    , FontVariant(..)
    , smallCaps
    -- * @font-weight@
    , FontWeight(..)
    , bold
    , bolder
    , lighter
    -- * @font-size@
    , FontSize(..)
    , xxSmall
    , xSmall
    , small
    , medium
    , large
    , xLarge
    , xxLarge
    , larger
    , smaller
    -- * @line-height@
    , LineHeight(..)
    -- * @font-family@
    , FontFamily(..)
    , serif
    , sansSerif
    , monospace
    , cursive
    , fantasy
    -- * Normal values
    , NormalProperty(..)
    ) where

import           Control.Applicative
import           Control.Monad

import           Data.CaseInsensitive (mk)
import           Data.Char
import           Data.Default.Class
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import           Data.Text (Text)

import           Graphics.Blank.Types.CSS
import           Graphics.Blank.Parser

import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.ParserCombinators.ReadP hiding ((<++), choice, pfail)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import           Text.ParserCombinators.ReadPrec (ReadPrec, (<++), lift, pfail)
import           Text.Read (Read(..), readListPrecDefault)

-------------------------------------------------------------------------------

-- | A CSS-style font data type.
data Font = FontProperties
  {   fontStyle   :: FontStyle
    , fontVariant :: FontVariant
    , fontWeight  :: FontWeight
    , fontSize    :: FontSize
    , lineHeight  :: LineHeight
    , fontFamily  :: [FontFamily]
  } -- ^ A font specified by its individual longhand properties.
  | CaptionFont      -- ^ The font used for captioned controls (e.g., buttons, drop-downs, etc.)
  | IconFont         -- ^ The font used to label icons.
  | MenuFont         -- ^ The font used in menus (e.g., dropdown menus and menu lists).
  | MessageBoxFont   -- ^ The font used in dialog boxes.
  | SmallCaptionFont -- ^ The font used for labeling small controls.
  | StatusBarFont    -- ^ The font used in window status bars.
  deriving Eq

-- |
-- Creates a new font from the 'FontFamily' list, using the 'Default' instances
-- for the other five longhand properties. If you only wish to change certain
-- properties and leave the others alone, this provides a convenient mechanism
-- for doing so:
-- 
-- @
-- ('defFont' ["Gill Sans Extrabold", 'sansSerif']) {
--     'fontStyle'  = 'italic'
--   , 'fontSize'   = 'px' 12
--   , 'lineHeight' = 'px' 14
-- }
-- @
defFont :: [FontFamily] -> Font
defFont = FontProperties def def def def def

-- | Shorthand for 'CaptionFont'.
caption :: Font
caption = CaptionFont

-- | Shorthand for 'IconFont'.
icon :: Font
icon = IconFont

-- | Shorthand for 'MenuFont'.
menu :: Font
menu = MenuFont

-- | Shorthand for 'MessageBoxFont'.
messageBox :: Font
messageBox = MessageBoxFont

-- | Shorthand for 'SmallCaptionFont'.
smallCaption :: Font
smallCaption = SmallCaptionFont

-- | Shorthand for 'StatusBarFont'.
statusBar :: Font
statusBar = StatusBarFont

instance IsString Font where
    fromString = read

instance Read Font where
    readPrec = do
        lift skipSpaces
        ReadPrec.choice
            [ CaptionFont      <$ lift (stringCI "caption")
            , IconFont         <$ lift (stringCI "icon")
            , MenuFont         <$ lift (stringCI "menu")
            , MessageBoxFont   <$ lift (stringCI "message-box")
            , SmallCaptionFont <$ lift (stringCI "small-caption")
            , StatusBarFont    <$ lift (stringCI "status-bar")
            , readFontProperties Nothing Nothing Nothing
            ]
    readListPrec = readListPrecDefault

-- | Like Either, but with three possibilities instead of two.
data OneOfThree a b c = One a | Two b | Three c

-- |
-- The formal syntax for the font CSS property
-- (https://developer.mozilla.org/en-US/docs/Web/CSS/font#Syntax) is surprisingly complex.
-- It requires that font-style, font-variant, and font-weight must be defined, if any,
-- before the font-size value. Furthermore, each of those three properties may only be defined at
-- most once, and the relative order of the three does not matter. This is a tall order for the
-- Text.ParserCombinators modules, so we use a heavily monadic utility function to detect
-- make it easier to catch bad input. The three Maybe arguments each represent whether its
-- respective property has not (Nothing) or has (Just) been read. If it has been read, then
-- readFontProperties will not attempt to parse it again.
-- 
-- readFontProperties will proceed to parse the remaining Font longhand properties once
-- either all three of the first properties have been parsed, or when it is unsuccessful at
-- parsing any of the first three properties.
readFontProperties :: Maybe FontStyle -> Maybe FontVariant -> Maybe FontWeight -> ReadPrec Font
readFontProperties style variant weight =
    -- If all three properties have been parsed, proceed to the remaining three properties.
    if isJust style && isJust variant && isJust weight
       then readFontProperties' style variant weight
       else do
               -- If the property has already been parsed, do not parse it again.
           let parseCheck :: Maybe a -> ReadPrec a -> ReadPrec a
               parseCheck mb parser = if isJust mb then pfail else parser
               
               readStyle, readVariant, readWeight :: ReadPrec (OneOfThree FontStyle FontVariant FontWeight)
               readStyle   = One   <$> parseCheck style readPrec
               readVariant = Two   <$> parseCheck variant readPrec
               readWeight  = Three <$> parseCheck weight readPrec
           
           -- First attempt to parse font-style, then font-variant, then font-weight (unless one
           -- of them has already been parsed, in which case skip to the next property parser.
           prop <- maybeRead $ readStyle <++ readVariant <++ readWeight
           -- Check to see which property, if any, was parsed.
           case prop of
               Just (One style') -> do
                   when (isJust style) pfail -- Safeguard to ensure a property is not parsed twice.
                   readFontProperties (Just style') variant weight
               Just (Two variant') -> do
                   when (isJust variant) pfail
                   readFontProperties style (Just variant') weight
               Just (Three weight') -> do
                   when (isJust weight) pfail
                   readFontProperties style variant (Just weight')
               -- If no properties were parsed, proceed to the remaining three properties.
               Nothing -> readFontProperties' style variant weight

-- |
-- Parses the remaining three Font longhand properties (font-size, line-height, and
-- font-family). Make sure to also parse the forward slash, if any, that separates
-- the font-size and line-height properties.
readFontProperties' :: Maybe FontStyle -> Maybe FontVariant -> Maybe FontWeight -> ReadPrec Font
readFontProperties' mbStyle mbVariant mbWeight =
  FontProperties (fromMaybe def mbStyle) (fromMaybe def mbVariant) (fromMaybe def mbWeight)
  <$> readPrec
  <*> lift (option def $ skipSpaces *> char '/' *> unlift readPrec)
  <*> (lift (munch1 isSpace) *> readPrec)

instance Show Font where
    showsPrec d (FontProperties style variant weight size height family)
        = showsPrec d style
        . showSpace
        . showsPrec d variant
        . showSpace
        . showsPrec d weight
        . showSpace
        . showsPrec d size
        . showChar '/'
        . showsPrec d height
        . showSpace
        . showsPrec d family
      where
        showSpace = showChar ' '
    showsPrec _ CaptionFont      = showString "caption"
    showsPrec _ IconFont         = showString "icon"
    showsPrec _ MenuFont         = showString "menu"
    showsPrec _ MessageBoxFont   = showString "message-box"
    showsPrec _ SmallCaptionFont = showString "small-caption"
    showsPrec _ StatusBarFont    = showString "status-bar"

-------------------------------------------------------------------------------

-- | Specifies if a 'Font' is italic or oblique.
data FontStyle = NormalStyle  -- ^ Selects a font classified as normal (default).
               | ItalicStyle  -- ^ Selects a font that is labeled italic, or if one is not available,
                              --   one labeled oblique.
               | ObliqueStyle -- ^ Selects a font that is labeled oblique.
  deriving Eq

-- | Shorthand for 'ItalicStyle'.
italic :: FontStyle
italic = ItalicStyle

-- | Shorthand for 'ObliqueStyle'.
oblique :: FontStyle
oblique = ObliqueStyle

instance Default FontStyle where
    def = NormalStyle

instance IsString FontStyle where
    fromString = read

instance NormalProperty FontStyle

instance Read FontStyle where
    readPrec = lift $ do
        skipSpaces
        ReadP.choice
            [ NormalStyle  <$ stringCI "normal"
            , ItalicStyle  <$ stringCI "italic"
            , ObliqueStyle <$ stringCI "oblique"
            ]
    readListPrec = readListPrecDefault

instance Show FontStyle where
    showsPrec _ fs = showString $ case fs of
        NormalStyle  -> "normal"
        ItalicStyle  -> "italic"
        ObliqueStyle -> "oblique"

-------------------------------------------------------------------------------

-- | Specifies the face of a 'Font'.
data FontVariant = NormalVariant    -- ^ A normal font face (default).
                 | SmallCapsVariant -- ^ A font face with small capital letters for lowercase characters.
  deriving Eq

-- | Shorthand for 'SmallCapsVariant'.
smallCaps :: FontVariant
smallCaps = SmallCapsVariant

instance Default FontVariant where
    def = NormalVariant

instance IsString FontVariant where
    fromString = read

instance NormalProperty FontVariant

instance Read FontVariant where
    readPrec = lift $ do
      skipSpaces
      (NormalVariant <$ stringCI "normal") <|> (SmallCapsVariant <$ stringCI "small-caps")
    readListPrec = readListPrecDefault

instance Show FontVariant where
    showsPrec _ fv = showString $ case fv of
        NormalVariant    -> "normal"
        SmallCapsVariant -> "small-caps"

-------------------------------------------------------------------------------

-- |
-- Specifies the boldness of a 'Font'. Note that 'FontWeight' is an instance of
-- 'Num' so that the nine numeric weights can be used directly. For example:
-- 
-- @
-- ('defFont' ['sansSerif']) { 'fontWeight' = 900 }
-- @
-- 
-- Attempting to use a numeric weight other than the nine given will result in
-- a runtime error.
data FontWeight = NormalWeight -- ^ Default.
                | BoldWeight
                | BolderWeight
                | LighterWeight
                | Weight100
                | Weight200
                | Weight300
                | Weight400
                | Weight500
                | Weight600
                | Weight700
                | Weight800
                | Weight900
  deriving Eq

-- | Shorthand for 'BoldWeight'.
bold :: FontWeight
bold = BoldWeight

-- | Shorthand for 'BolderWeight'.
bolder :: FontWeight
bolder = BolderWeight

-- | Shorthand for 'LighterWeight'.
lighter :: FontWeight
lighter = LighterWeight

fontWeightError :: a
fontWeightError = error "invalid font-weight operation"

instance Default FontWeight where
    def = NormalWeight

instance IsString FontWeight where
    fromString = read

instance NormalProperty FontWeight

instance Num FontWeight where
    (+)    = fontWeightError
    (-)    = fontWeightError
    (*)    = fontWeightError
    abs    = fontWeightError
    signum = fontWeightError
    fromInteger 100 = Weight100
    fromInteger 200 = Weight200
    fromInteger 300 = Weight300
    fromInteger 400 = Weight400
    fromInteger 500 = Weight500
    fromInteger 600 = Weight600
    fromInteger 700 = Weight700
    fromInteger 800 = Weight800
    fromInteger 900 = Weight900
    fromInteger _   = fontWeightError

instance Read FontWeight where
    readPrec = lift $ do
        skipSpaces
        ReadP.choice
            [ NormalWeight  <$ stringCI "normal"
            , BoldWeight    <$ stringCI "bold"
            , BolderWeight  <$ stringCI "bolder"
            , LighterWeight <$ stringCI "lighter"
            , Weight100     <$ string   "100"
            , Weight200     <$ string   "200"
            , Weight300     <$ string   "300"
            , Weight400     <$ string   "400"
            , Weight500     <$ string   "500"
            , Weight600     <$ string   "600"
            , Weight700     <$ string   "700"
            , Weight800     <$ string   "800"
            , Weight900     <$ string   "900"
            ]
    readListPrec = readListPrecDefault

instance Show FontWeight where
    showsPrec _ fw = showString $ case fw of
          NormalWeight  -> "normal"
          BoldWeight    -> "bold"
          BolderWeight  -> "bolder"
          LighterWeight -> "lighter"
          Weight100     -> "100"
          Weight200     -> "200"
          Weight300     -> "300"
          Weight400     -> "400"
          Weight500     -> "500"
          Weight600     -> "600"
          Weight700     -> "700"
          Weight800     -> "800"
          Weight900     -> "900"

-------------------------------------------------------------------------------

-- | The desired height of 'Font' glyphs. Examples:
-- 
-- @
-- ('defFont' ['sansSerif']) { 'fontSize' = 'xxSmall' }
-- ('defFont' ['sansSerif']) { 'fontSize' = 'pt' 30 }
-- ('defFont' ['sansSerif']) { 'fontSize' = 'percent' 50 }
-- @
data FontSize = XXSmallSize
              | XSmallSize
              | SmallSize
              | MediumSize -- ^ Default.
              | LargeSize
              | XLargeSize
              | XXLargeSize
              | LargerSize
              | SmallerSize
              | FontSizeLength Length
              | FontSizePercentage Percentage
  deriving Eq

-- | Shorthand for 'XXSmallSize'.
xxSmall :: FontSize
xxSmall = XXSmallSize

-- | Shorthand for 'XSmallSize'.
xSmall :: FontSize
xSmall = XSmallSize

-- | Shorthand for 'SmallSize'.
small :: FontSize
small = SmallSize

-- | Shorthand for 'MediumSize'.
medium :: FontSize
medium = MediumSize

-- | Shorthand for 'LargeSize'.
large :: FontSize
large = LargeSize

-- | Shorthand for 'XLargeSize'.
xLarge :: FontSize
xLarge = XLargeSize

-- | Shorthand for 'XXLargeSize'.
xxLarge :: FontSize
xxLarge = XXLargeSize

-- | Shorthand for 'LargerSize'.
larger :: FontSize
larger = LargerSize

-- | Shorthand for 'SmallerSize'.
smaller :: FontSize
smaller = SmallerSize

instance Default FontSize where
    def = MediumSize

instance IsString FontSize where
    fromString = read

instance LengthProperty FontSize where
    fromLength = FontSizeLength

instance PercentageProperty FontSize where
    percent = FontSizePercentage

instance Read FontSize where
    readPrec = do
        lift $ skipSpaces
        ReadPrec.choice
            [ XXSmallSize        <$  lift (stringCI "xx-small")
            , XSmallSize         <$  lift (stringCI "x-small")
            , SmallSize          <$  lift (stringCI "small")
            , MediumSize         <$  lift (stringCI "medium")
            , LargeSize          <$  lift (stringCI "large")
            , XLargeSize         <$  lift (stringCI "x-large")
            , XXLargeSize        <$  lift (stringCI "xx-large")
            , LargerSize         <$  lift (stringCI "larger")
            , SmallerSize        <$  lift (stringCI "smaller")
            , FontSizeLength     <$> readPrec
            , FontSizePercentage <$> readPrec <* lift (char '%')
            ]
    readListPrec = readListPrecDefault

instance Show FontSize where
    showsPrec _ XXSmallSize            = showString "xx-small"
    showsPrec _ XSmallSize             = showString "x-small"
    showsPrec _ SmallSize              = showString "small"
    showsPrec _ MediumSize             = showString "medium"
    showsPrec _ LargeSize              = showString "large"
    showsPrec _ XLargeSize             = showString "x-large"
    showsPrec _ XXLargeSize            = showString "xx-large"
    showsPrec _ LargerSize             = showString "larger"
    showsPrec _ SmallerSize            = showString "smaller"
    showsPrec d (FontSizeLength l)     = showsPrec d l
    showsPrec _ (FontSizePercentage p) = jsDoubleS p . showChar '%'

-------------------------------------------------------------------------------

-- | The height of the line boxes in a 'Font'. Examples:
-- 
-- @
-- ('defFont' ['sansSerif']) { 'lineHeight' = 'normal' }
-- ('defFont' ['sansSerif']) { 'lineHeight' = 50 }
-- ('defFont' ['sansSerif']) { 'lineHeight' = 'em' 30 }
-- ('defFont' ['sansSerif']) { 'lineHeight' = 'percent' 70 }
-- @
data LineHeight = NormalLineHeight -- ^ Default.
                | LineHeightNumber Double
                | LineHeightLength Length
                | LineHeightPercentage Percentage
  deriving Eq

lineHeightError :: a
lineHeightError = error "no arithmetic for line-height"

instance Default LineHeight where
    def = NormalLineHeight

instance Fractional LineHeight where
    (/)   = lineHeightError
    recip = lineHeightError
    fromRational = LineHeightNumber . fromRational

instance IsString LineHeight where
    fromString = read

instance LengthProperty LineHeight where
    fromLength = LineHeightLength

instance NormalProperty LineHeight

instance Num LineHeight where
    (+)    = lineHeightError
    (-)    = lineHeightError
    (*)    = lineHeightError
    abs    = lineHeightError
    signum = lineHeightError
    fromInteger = LineHeightNumber . fromInteger

instance PercentageProperty LineHeight where
    percent = LineHeightPercentage

instance Read LineHeight where
    readPrec = do
        lift skipSpaces
        ReadPrec.choice
            [ NormalLineHeight     <$  lift (stringCI "normal")
            , LineHeightNumber     <$> readPrec
            , LineHeightLength     <$> readPrec
            , LineHeightPercentage <$> readPrec <* lift (char '%')
            ]
    readListPrec = readListPrecDefault

instance Show LineHeight where
    showsPrec _ NormalLineHeight         = showString "normal"
    showsPrec _ (LineHeightNumber n)     = jsDoubleS n
    showsPrec d (LineHeightLength l)     = showsPrec d l
    showsPrec _ (LineHeightPercentage p) = jsDoubleS p . showChar '%'

-------------------------------------------------------------------------------

-- |
-- The name of a 'Font' family. Note that both 'FontFamily' and @['FontFamily']@
-- are instances of 'IsString', so it is possible to produce 'FontFamily' values
-- in several different ways. For example, these are all of type 'FontFamily':
-- 
-- @
-- 'FontFamilyName' "Gill Sans Extrabold"
-- "Gill Sans Extrabold" :: 'FontFamily'
-- 'serif'
-- "serif" :: 'FontFamily'
-- @
-- 
-- These are all of type @['FontFamily']@:
-- 
-- @
-- ['FontFamilyName' "Helvetica", 'serif']
-- ["Helvetica", "serif"] :: ['FontFamily']
-- "Helvetica, serif" :: ['FontFamily']
-- @
data FontFamily = FontFamilyName Text -- ^ The name of a custom font family.
                | SerifFamily         -- ^ A generic font family where glyphs have
                                      --   serifed endings.
                | SansSerifFamily     -- ^ A generic font family where glyphs do not
                                      --   have serifed endings.
                | MonospaceFamily     -- ^ A generic font family where all glyphs have
                                      --   the same fixed width.
                | CursiveFamily       -- ^ A generic font family with cursive glyphs.
                | FantasyFamily       -- ^ A generic font family where glyphs have
                                      --   decorative, playful representations.
  deriving Eq

-- | Shorthand for 'SerifFamily'.
serif :: FontFamily
serif = SerifFamily

-- | Shorthand for 'SansSerifFamily'.
sansSerif :: FontFamily
sansSerif = SansSerifFamily

-- | Shorthand for 'MonospaceFamily'.
monospace :: FontFamily
monospace = MonospaceFamily

-- | Shorthand for 'CursiveFamily'.
cursive :: FontFamily
cursive = CursiveFamily

-- | Shorthand for 'FantasyFamily'.
fantasy :: FontFamily
fantasy = FantasyFamily

-- |
-- Make sure to have two separate IsString instances so that single font families
-- and lists of font families alike can be converted from string literals.
instance IsString FontFamily where
    fromString = read

instance IsString [FontFamily] where
    fromString = read

instance Read FontFamily where
    readPrec = lift $ do
        skipSpaces
        let quoted = between (char '"') (char '"')
        quoted (readFontFamily True) <|> readFontFamily False
    
    -- readListPrec is overloaded so that it will read in a comma-separated list of
    -- family names not delimited by square brackets, as per the CSS syntax.
    readListPrec = lift . sepBy1 (unlift readPrec) $ skipSpaces *> char ','

-- |
-- FontFamilyNames can either be a series of whitespace-separated CSS identifiers
-- (parsed with cssIdent) or a series of characters delimited by quotation marks.
-- Use the Bool argument to distinguish between the two.
readFontFamily :: Bool -> ReadP FontFamily
readFontFamily quoted = do
    name <- if quoted
               then munch (/= '\"')
               else unwords <$> sepBy1 cssIdent (munch1 isSpace)
    return $ case () of
         _ | mk name == mk "serif"      -> SerifFamily
         _ | mk name == mk "sans-serif" -> SansSerifFamily
         _ | mk name == mk "monospace"  -> MonospaceFamily
         _ | mk name == mk "cursive"    -> CursiveFamily
         _ | mk name == mk "fantasy"    -> FantasyFamily
         _otherwise                     -> FontFamilyName $ T.pack name

instance Show FontFamily where
    showsPrec d (FontFamilyName name) = showsPrec d name
    showsPrec _ SerifFamily           = showString "serif"
    showsPrec _ SansSerifFamily       = showString "sans-serif"
    showsPrec _ MonospaceFamily       = showString "monospace"
    showsPrec _ CursiveFamily         = showString "cursive"
    showsPrec _ FantasyFamily         = showString "fantasy"
    
    -- Omit the square brackets when showing a list of font families so that
    -- it matches the CSS syntax.
    showList = mconcat . intersperse (showChar ',') . map shows

-------------------------------------------------------------------------------

-- | A convenient way to use the 'Default' normal value for several Font longhand
--   properties.
class Default a => NormalProperty a where
    normal :: a
    normal = def