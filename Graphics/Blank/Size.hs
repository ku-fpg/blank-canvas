module Graphics.Blank.Size where

class Size a where
   width  :: a -> Int
   height :: a -> Int

