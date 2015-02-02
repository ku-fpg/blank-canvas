module Graphics.Blank.Font
    ( -- * Overloaded @font@
      font
    , CanvasFont(..)
      -- * @font@
    , Font(..)
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
    -- * Lengths
    , Length(..)
    , LengthProperty(..)
    , em
    , ex
    , ch
    , rem
    , vh
    , vw
    , vmin
    , vmax
    , px
    , mm
    , cm
    , in_
    , pt
    , pc
    -- * Percentages
    , Percentage
    , PercentageProperty(..)
    ) where

import Graphics.Blank.Generated (font)
import Graphics.Blank.Types.CSS
import Graphics.Blank.Types.Font

import Prelude hiding (rem)
