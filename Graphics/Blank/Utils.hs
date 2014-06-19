module Graphics.Blank.Utils where

import Graphics.Blank.Canvas
import Graphics.Blank.Generated
import Graphics.Blank.Size
import Graphics.Blank.Context

-- | Clear the screen. Restores the default transformation matrix.
clearCanvas :: Context -> Canvas ()
clearCanvas context = do
  setTransform (1, 0, 0, 1, 0, 0)
  clearRect (0,0,width context,height context)

-- | Wrap a canvas computation in 'save' / 'restore'.
saveRestore :: Canvas () -> Canvas ()
saveRestore m = do
    save ()
    m
    restore ()
    
    
infixr 0 #

-- | The @#@-operator is the Haskell analog to the @.@-operator
--   in Javascript. Example:
--
-- > grd # addColorStop(0, "#8ED6FF");
--
--   This can be seen as equivalent of @document.addColorStop(0, "#8ED6FF")@.
(#) :: a -> (a -> Canvas b) -> Canvas b
(#) obj act = act obj
