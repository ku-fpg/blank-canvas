{-|
Module:      Graphics.Blank.Cursor
Copyright:   (C) 2014-2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Beta
Portability: GHC

This module exposes an overloaded version of the 'cursor' function that can accept
a 'Cursor' ADT argument. This may be of interest if you desire stronger type safety
than @Text@-based cursors provide.

Note that this module's 'cursor' function conflicts with @cursor@ from
"Graphics.Blank". Make sure to hide @cursor@ from "Graphics.Blank" if you use
'cursor' from this module.
-}
module Graphics.Blank.Cursor (
      -- * Overloaded @cursor@
      cursor
    , CanvasCursor(..)
      -- Cursors
    , Cursor(..)
    , auto
    , default_
    , none
    , contextMenu
    , help
    , pointer
    , progress
    , wait
    , cell
    , crosshair
    , text
    , verticalText
    , alias
    , copy
    , move
    , noDrop
    , notAllowed
    , allScroll
    , colResize
    , rowResize
    , nResize
    , eResize
    , sResize
    , wResize
    , neResize
    , nwResize
    , seResize
    , swResize
    , ewResize
    , nsResize
    , neswResize
    , nwseResize
    , zoomIn
    , zoomOut
    , grab
    , grabbing
    , url
    , urlXY
    ) where

import Graphics.Blank.Canvas (cursor)
import Graphics.Blank.Types.Cursor