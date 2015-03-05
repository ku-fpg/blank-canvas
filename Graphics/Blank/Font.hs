{-|
Module:      Graphics.Blank.Font
Copyright:   (C) 2014-2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Beta
Portability: GHC

This module exposes an overloaded version of the 'font' function that can accept
a 'Font' ADT argument. This may be of interest if you desire stronger type safety
than @Text@-based fonts provide.

Note that this module's 'font' function conflicts with @font@ from "Graphics.Blank".
Make sure to hide @font@ from "Graphics.Blank" if you use 'font' from this module.
-}
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
    , PercentageProperty(..)
    ) where

import Graphics.Blank.Generated (font)
import Graphics.Blank.Types.CSS
import Graphics.Blank.Types.Font

import Prelude hiding (rem)
