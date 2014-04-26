module Graphics.Blank.Utils where

import Graphics.Blank.Canvas
import Graphics.Blank.Generated

-- | Clear the screen. Restores the default transformation matrix.
clearCanvas :: Canvas ()
clearCanvas = do
  transform (1, 0, 0, 1, 0, 0)
  (width,height) <- size
  clearRect (0,0,width,height)

-- | Wrap a canvas computation in save / restore.
saveRestore :: Canvas () -> Canvas ()
saveRestore m = do
    save ()
    m
    restore ()
    
    