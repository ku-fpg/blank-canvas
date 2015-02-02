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
    ) where

import Graphics.Blank.Canvas (cursor)
import Graphics.Blank.Types.Cursor