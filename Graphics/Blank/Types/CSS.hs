{-# LANGUAGE OverloadedStrings, TypeSynonymInstances #-}
module Graphics.Blank.Types.CSS where

import           Data.Functor ((<$))
import           Data.Monoid ((<>))
import           Data.String

import           Graphics.Blank.JavaScript
import           Graphics.Blank.Parser

import           Prelude hiding (Show, rem)

import           Text.ParserCombinators.ReadP (choice)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..), readListPrecDefault)
import qualified Text.Show as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text (FromTextShow(..), showb)

-- | Denotes CSS distance measurements, especially in the context of 'Font's.
data Length = Em   { runLength :: Double } -- ^ The height of the current font.
            | Ex   { runLength :: Double } -- ^ The height of the character @x@ (x-height) in the current font.
            | Ch   { runLength :: Double } -- ^ The width of the character @0@ in the current font.
            | Rem  { runLength :: Double } -- ^ The height of the font relative to the root element.
            | Vh   { runLength :: Double } -- ^ One percent of the height of the viewport.
            | Vw   { runLength :: Double } -- ^ One percent of the width of the viewport.
            | Vmin { runLength :: Double } -- ^ One percent of the minimum of the viewport height and width.
            | Vmax { runLength :: Double } -- ^ One percent of the maximum of the viewport height and width.
            | Px   { runLength :: Double } -- ^ One device pixel (dot) of the display.
            | Mm   { runLength :: Double } -- ^ One millimeter.
            | Cm   { runLength :: Double } -- ^ One centimeter (10 millimeters).
            | In   { runLength :: Double } -- ^ One inch (~2.54 centimeters).
            | Pt   { runLength :: Double } -- ^ One point (1/72 inches).
            | Pc   { runLength :: Double } -- ^ One pica (12 points).
  deriving (Eq, Ord)

-- | Designates CSS properties that can consist of a 'Length'.
class LengthProperty a where
    fromLength :: Length -> a

instance LengthProperty Length where
    fromLength = id

-- | Constructs a 'LengthProperty' value with 'Em' units.
em :: LengthProperty a => Double -> a
em = fromLength . Em

-- | Constructs a 'LengthProperty' value with 'Ex' units.
ex :: LengthProperty a => Double -> a
ex = fromLength . Ex

-- | Constructs a 'LengthProperty' value with 'Ch' units.
ch :: LengthProperty a => Double -> a
ch = fromLength . Ch

-- | Constructs a 'LengthProperty' value with 'Rem' units.
rem :: LengthProperty a => Double -> a
rem = fromLength . Rem

-- | Constructs a 'LengthProperty' value with 'Vh' units.
vh :: LengthProperty a => Double -> a
vh = fromLength . Vh

-- | Constructs a 'LengthProperty' value with 'Vw' units.
vw :: LengthProperty a => Double -> a
vw = fromLength . Vw

-- | Constructs a 'LengthProperty' value with 'Em' units.
vmin :: LengthProperty a => Double -> a
vmin = fromLength . Vmin

-- | Constructs a 'LengthProperty' value with 'Vmax' units.
vmax :: LengthProperty a => Double -> a
vmax = fromLength . Vmax

-- | Constructs a 'LengthProperty' value with 'Px' units.
px :: LengthProperty a => Double -> a
px = fromLength . Px

-- | Constructs a 'LengthProperty' value with 'Mm' units.
mm :: LengthProperty a => Double -> a
mm = fromLength . Mm

-- | Constructs a 'LengthProperty' value with 'Cm' units.
cm :: LengthProperty a => Double -> a
cm = fromLength . Cm

-- | Constructs a 'LengthProperty' value with 'Im' units. This function has an
--   underscore to distinguish it from the Haskell keyword.
in_ :: LengthProperty a => Double -> a
in_ = fromLength . In

-- | Constructs a 'LengthProperty' value with 'Pt' units.
pt :: LengthProperty a => Double -> a
pt = fromLength . Pt

-- | Constructs a 'LengthProperty' value with 'Pc' units.
pc :: LengthProperty a => Double -> a
pc = fromLength . Pc

instance IsString Length where
    fromString = read

instance Read Length where
    readPrec = do
        d <- readPrec
        lift $ choice
            [ Em d   <$ stringCI "em"
            , Ex d   <$ stringCI "ex"
            , Ch d   <$ stringCI "ch"
            , Rem d  <$ stringCI "rem"
            , Vh d   <$ stringCI "vh"
            , Vw d   <$ stringCI "vw"
            , Vmin d <$ stringCI "vmin"
            , Vmax d <$ stringCI "vmax"
            , Px d   <$ stringCI "px"
            , Mm d   <$ stringCI "mm"
            , Cm d   <$ stringCI "cm"
            , In d   <$ stringCI "in"
            , Pt d   <$ stringCI "pt"
            , Pc d   <$ stringCI "pc"
            ]
    readListPrec = readListPrecDefault

instance S.Show Length where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show Length where
    showb l = jsDouble (runLength l) <> showbUnits l
      where
        showbUnits (Em   _) = "em"
        showbUnits (Ex   _) = "ex"
        showbUnits (Ch   _) = "ch"
        showbUnits (Rem  _) = "rem"
        showbUnits (Vh   _) = "vh"
        showbUnits (Vw   _) = "vw"
        showbUnits (Vmin _) = "vmin"
        showbUnits (Vmax _) = "vmax"
        showbUnits (Px   _) = "px"
        showbUnits (Mm   _) = "mm"
        showbUnits (Cm   _) = "cm"
        showbUnits (In   _) = "in"
        showbUnits (Pt   _) = "pt"
        showbUnits (Pc   _) = "pc"

-- | A value ranging from 0.0 to 100.0.
type Percentage = Double

-- | Designates CSS properties that can consist of a 'Percentage'.
class PercentageProperty a where
    percent :: Percentage -> a

instance PercentageProperty Percentage where
    percent = id
