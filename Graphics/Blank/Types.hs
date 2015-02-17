module Graphics.Blank.Types where

-- | A value ranging from 0.0 to 1.0.
type Interval = Double

-- | An interval representing a color's translucency. A color with an alpha value
-- of 0.0 is 'transparent', and a color with an alpha value of 1.0 is opaque.
type Alpha = Interval

-- | An angle type in which 360° represents one complete rotation.
type Degrees = Double

-- | An angle type in which 2π radians represents one complete rotation.
type Radians = Double