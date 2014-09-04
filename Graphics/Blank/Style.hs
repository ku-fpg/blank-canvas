module Graphics.Blank.Style 
        ( -- * Overloaded versions of 'Canvas' functions
          strokeStyle
        , fillStyle
        , shadowColor
        , addColorStop
        , Style
        , CanvasColor
        -- * 'CanvasColor' creation
        , Alpha
        , Percentage
        , rgb
        , rgbPercent
        , rgba
        , rgbaPercent
        , hsl
        , hsla
        ) where

import           Data.Colour
import           Data.Colour.RGBSpace
import qualified Data.Colour.RGBSpace.HSL as HSL
import           Data.Colour.SRGB
import qualified Data.Colour.SRGB.Linear as Linear

import           Graphics.Blank.Canvas
import           Graphics.Blank.Generated
import           Graphics.Blank.JavaScript

-- |
-- A value ranging from 0.0 to 1.0. A color with an alpha value of 0.0 is 'transparent',
-- and a color with an alpha value of 1.0 is opaque.
type Alpha = Float

-- |
-- A value ranging from 0.0 to 100.0.
type Percentage = Float

-- |
-- Specifies a 'Colour' by its red, green, and blue components, where each component
-- is an integer between 0 and 255.
rgb :: Int -> Int -> Int -> Colour Float
rgb r g b = sRGB (realToFrac r) (realToFrac b) (realToFrac g)

-- |
-- Specifies a 'Colour' by its red, green, and blue components, where each component
-- is given by a percentage of 255.
rgbPercent :: Percentage -> Percentage -> Percentage -> Colour Float
rgbPercent r g b = Linear.rgb (r/100) (g/100) (b/100)

-- |
-- Specifies an `AlphaColour' by its RGB components and an alpha value.
-- 
-- @
-- 'rgba' r g b 0.0 = 'transparent`
-- @
rgba :: Int -> Int -> Int -> Alpha -> AlphaColour Float
rgba r g b = withOpacity $ rgb r g b

-- |
-- Specifies an 'AlphaColour' by its RGB component percentages and an alpha value.
-- 
-- @
-- 'rgbaPercent' r g b 0.0 = 'transparent`
-- @
rgbaPercent :: Percentage -> Percentage -> Percentage -> Alpha -> AlphaColour Float
rgbaPercent r g b = withOpacity $ rgbPercent r g b

-- |
-- Specifies a 'Colour' by its hue (which ranges from 0° to 360°), saturation, and
-- value.
hsl :: Int -> Percentage -> Percentage -> Colour Float
hsl h s l = uncurryRGB sRGB $ HSL.hsl (realToFrac h) (s/100) (l/100)

-- |
-- Specifies an 'AlphaColour' by its HSV values and an alpha value.
-- 
-- @
-- 'hsla' h s v 0.0 = 'transparent'
-- @
hsla :: Int -> Percentage -> Percentage -> Alpha -> AlphaColour Float
hsla h s l = withOpacity $ hsl h s l