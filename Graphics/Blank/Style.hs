{-|
Module:      Graphics.Blank.Style
Copyright:   (C) 2014-2015, The University of Kansas
License:     BSD-style (see the file LICENSE)
Maintainer:  Andy Gill
Stability:   Beta
Portability: GHC

This module exposes overloaded versions of @blank-canvas@ functions that take a
style or color as an argument, which may be of interest if you desire stronger
type safety than @Text@ provides.

Note that this module exports function names that conflict with "Graphics.Blank".
Make sure to hide any functions from "Graphics.Blank" that you use from this
module.
-}
module Graphics.Blank.Style 
    ( -- * Overloaded versions of 'Canvas' functions
      strokeStyle
    , fillStyle
    , shadowColor
    , addColorStop
    , Style(..)
    , CanvasColor
      -- * 'CanvasColor' creation
    , rgb
    , rgbPercent
    , rgba
    , rgbaPercent
    , hsl
    , hsla
      -- * @colour@ reexports
    , Colour
    , AlphaColour
    , readColourName
    , aliceblue
    , antiquewhite
    , aqua
    , aquamarine
    , azure
    , beige
    , bisque
    , black
    , blanchedalmond
    , blue
    , blueviolet
    , brown
    , burlywood
    , cadetblue
    , chartreuse
    , chocolate
    , coral
    , cornflowerblue
    , cornsilk
    , crimson
    , cyan
    , darkblue
    , darkcyan
    , darkgoldenrod
    , darkgray
    , darkgreen
    , darkgrey
    , darkkhaki
    , darkmagenta
    , darkolivegreen
    , darkorange
    , darkorchid
    , darkred
    , darksalmon
    , darkseagreen
    , darkslateblue
    , darkslategray
    , darkslategrey
    , darkturquoise
    , darkviolet
    , deeppink
    , deepskyblue
    , dimgray
    , dimgrey
    , dodgerblue
    , firebrick
    , floralwhite
    , forestgreen
    , fuchsia
    , gainsboro
    , ghostwhite
    , gold
    , goldenrod
    , gray
    , green
    , grey
    , greenyellow
    , honeydew
    , hotpink
    , indianred
    , indigo
    , ivory
    , khaki
    , lavender
    , lavenderblush
    , lawngreen
    , lemonchiffon
    , lightblue
    , lightcoral
    , lightcyan
    , lightgoldenrodyellow
    , lightgray
    , lightgreen
    , lightgrey
    , lightpink
    , lightsalmon
    , lightseagreen
    , lightskyblue
    , lightslategray
    , lightslategrey
    , lightsteelblue
    , lightyellow
    , lime
    , limegreen
    , linen
    , magenta
    , maroon
    , mediumaquamarine
    , mediumblue
    , mediumorchid
    , mediumpurple
    , mediumseagreen
    , mediumslateblue
    , mediumspringgreen
    , mediumturquoise
    , mediumvioletred
    , midnightblue
    , mintcream
    , mistyrose
    , moccasin
    , navajowhite
    , navy
    , oldlace
    , olive
    , olivedrab
    , orange
    , orangered
    , orchid
    , palegoldenrod
    , palegreen
    , paleturquoise
    , palevioletred
    , papayawhip
    , peachpuff
    , peru
    , pink
    , plum
    , powderblue
    , purple
    , red
    , rosybrown
    , royalblue
    , saddlebrown
    , salmon
    , sandybrown
    , seagreen
    , seashell
    , sienna
    , silver
    , skyblue
    , slateblue
    , slategray
    , slategrey
    , snow
    , springgreen
    , steelblue
    , tan
    , teal
    , thistle
    , tomato
    , turquoise
    , violet
    , wheat
    , white
    , whitesmoke
    , yellow
    , yellowgreen
    , rebeccapurple
    , transparent
    ) where

import qualified Data.Colour as Colour
import           Data.Colour hiding (black, transparent)
import qualified Data.Colour.Names as Names
import           Data.Colour.RGBSpace
import qualified Data.Colour.RGBSpace.HSL as HSL
import           Data.Colour.SRGB
import           Data.Word

import           Graphics.Blank.Canvas
import           Graphics.Blank.Generated
import           Graphics.Blank.JavaScript
import           Graphics.Blank.Types
import           Graphics.Blank.Types.CSS

import           Prelude hiding (tan)

-- | Specifies a 'Colour' by its red, green, and blue components, where each component
-- is an integer between 0 and 255.
rgb :: Word8 -> Word8 -> Word8 -> Colour Double
rgb = sRGB24

-- | Specifies a 'Colour' by its red, green, and blue components, where each component
-- is given by a percentage (which should be between 0% to 100%) of 255.
rgbPercent :: Percentage -> Percentage -> Percentage -> Colour Double
rgbPercent r g b = sRGB (r/100) (g/100) (b/100)

-- | Specifies an 'AlphaColour' by its RGB components and an alpha value.
-- 
-- @
-- 'rgba' r g b 0.0 = 'transparent'
-- @
rgba :: Word8 -> Word8 -> Word8 -> Alpha -> AlphaColour Double
rgba r g b = withOpacity $ rgb r g b

-- | Specifies an 'AlphaColour' by its RGB component percentages (which should be
-- between 0% and 100%) and an alpha value.
-- 
-- @
-- 'rgbaPercent' r g b 0.0 = 'transparent'
-- @
rgbaPercent :: Percentage -> Percentage -> Percentage -> Alpha -> AlphaColour Double
rgbaPercent r g b = withOpacity $ rgbPercent r g b

-- | Specifies a 'Colour' by its hue, saturation, and lightness value, where
-- saturation and lightness are percentages between 0% and 100%.
hsl :: Degrees -> Percentage -> Percentage -> Colour Double
hsl h s l = uncurryRGB sRGB $ HSL.hsl (realToFrac h) (s/100) (l/100)

-- |
-- Specifies an 'AlphaColour' by its HSL values and an alpha value.
-- 
-- @
-- 'hsla' h s v 0.0 = 'transparent'
-- @
hsla :: Degrees -> Percentage -> Percentage -> Alpha -> AlphaColour Double
hsla h s l = withOpacity $ hsl h s l

-- |
-- Takes a string naming a 'Colour' (must be all lowercase) and returns it. Fails if
-- the name is not recognized.
readColourName :: Monad m => String -> m (Colour Double)
readColourName "rebeccapurple" = return rebeccapurple
readColourName name            = Names.readColourName name

-- | @#F0F8FF@, @rgb(240, 248, 255)@, @hsl(208, 100%, 97%)@
aliceblue :: Colour Double
aliceblue = Names.aliceblue

-- | @#FAEBD7@, @rgb(250, 235, 215)@, @hsl(34, 78%, 91%)@
antiquewhite :: Colour Double
antiquewhite = Names.antiquewhite

-- | @#00FFFF@, @rgb(0, 255, 255)@, @hsl(180, 100%, 50%)@. Same as 'cyan'.
aqua :: Colour Double
aqua = Names.aqua

-- | @#7FFFD4@, @rgb(127, 255, 212)@, @hsl(160, 100%, 75%)@
aquamarine :: Colour Double
aquamarine = Names.aquamarine

-- | @#F0FFFF@, @rgb(240, 255, 255)@, @hsl(180, 100%, 97%)@
azure :: Colour Double
azure = Names.azure

-- | @#F5F5DC@, @rgb(245, 245, 220)@, @hsl(60, 56%, 91%)@
beige :: Colour Double
beige = Names.beige

-- | @#FFE4C4@, @rgb(255, 228, 196)@, @hsl(33, 100%, 88%)@
bisque :: Colour Double
bisque = Names.bisque

-- | @#000000@, @rgb(0, 0, 0)@, @hsl(0, 0%, 0%)@
black :: Colour Double
black = Colour.black

-- | @#FFEBCD@, @rgb(255, 235, 205)@, @hsl(36, 100%, 90%)@
blanchedalmond :: Colour Double
blanchedalmond = Names.blanchedalmond

-- | @#0000FF@, @rgb(0, 0, 255)@, @hsl(240, 100%, 50%)@
blue :: Colour Double
blue = Names.blue

-- | @#8A2BE2@, @rgb(138, 43, 226)@, @hsl(271, 76%, 53%)@
blueviolet :: Colour Double
blueviolet = Names.blueviolet

-- | @#A52A2A@, @rgb(165, 42, 42)@, @hsl(0, 59%, 41%)@
brown :: Colour Double
brown = Names.brown

-- | @#DEB887@, @rgb(222, 184, 135)@, @hsl(34, 57%, 70%)@
burlywood :: Colour Double
burlywood = Names.burlywood

-- | @#5F9EA0@, @rgb(95, 158, 160)@, @hsl(182, 25%, 50%)@
cadetblue :: Colour Double
cadetblue = Names.cadetblue

-- | @#7FFF00@, @rgb(127, 255, 0)@, @hsl(90, 100%, 50%)@
chartreuse :: Colour Double
chartreuse = Names.chartreuse

-- | @#D2691E@, @rgb(210, 105, 30)@, @hsl(25, 75%, 47%)@
chocolate :: Colour Double
chocolate = Names.chocolate

-- | @#FF7F50@, @rgb(255, 127, 80)@, @hsl(16, 100%, 66%)@
coral :: Colour Double
coral = Names.coral

-- | @#6495ED@, @rgb(100, 149, 237)@, @hsl(219, 79%, 66%)@
cornflowerblue :: Colour Double
cornflowerblue = Names.cornflowerblue

-- | @#FFF8DC@, @rgb(255, 248, 220)@, @hsl(48, 100%, 93%)@
cornsilk :: Colour Double
cornsilk = Names.cornsilk

-- | @#DC143C@, @rgb(220, 20, 60)@, @hsl(348, 83%, 58%)@
crimson :: Colour Double
crimson = Names.crimson

-- | @#00FFFF@, @rgb(0, 255, 255)@, @hsl(180, 100%, 50%)@. Same as 'aqua'.
cyan :: Colour Double
cyan = Names.cyan

-- | @#00008B@, @rgb(0, 0, 139)@, @hsl(240, 100%, 27%)@
darkblue :: Colour Double
darkblue = Names.darkblue

-- | @#008B8B@, @rgb(0, 139, 139)@, @hsl(180, 100%, 27%)@
darkcyan :: Colour Double
darkcyan = Names.darkcyan

-- | @#B8860B@, @rgb(184, 134, 11)@, @hsl(43, 89%, 38%)@
darkgoldenrod :: Colour Double
darkgoldenrod = Names.darkgoldenrod

-- | @#A9A9A9@, @rgb(169, 169, 169)@, @hsl(0, 0%, 66%)@. Same as 'darkgrey'.
darkgray :: Colour Double
darkgray = Names.darkgray

-- | @#006400@, @rgb(0, 100, 0)@, @hsl(120, 100%, 20%)@
darkgreen :: Colour Double
darkgreen = Names.darkgreen

-- | @#A9A9A9@, @rgb(169, 169, 169)@, @hsl(0, 0%, 66%)@. Same as 'darkgray'.
darkgrey :: Colour Double
darkgrey = Names.darkgrey

-- | @#BDB76B@, @rgb(189, 183, 107)@, @hsl(56, 38%, 58%)@
darkkhaki :: Colour Double
darkkhaki = Names.darkkhaki

-- | @#8B008B@, @rgb(139, 0, 139)@, @hsl(300, 100%, 27%)@
darkmagenta :: Colour Double
darkmagenta = Names.darkmagenta

-- | @#556B2F@, @rgb(85, 107, 47)@, @hsl(82, 39%, 30%)@
darkolivegreen :: Colour Double
darkolivegreen = Names.darkolivegreen

-- | @#FF8C00@, @rgb(255, 140, 0)@, @hsl(33, 100%, 50%)@
darkorange :: Colour Double
darkorange = Names.darkorange

-- | @#9932CC@, @rgb(153, 50, 204)@, @hsl(280, 61%, 50%)@
darkorchid :: Colour Double
darkorchid = Names.darkorchid

-- | @#8B0000@, @rgb(139, 0, 0)@, @hsl(0, 100%, 27%)@
darkred :: Colour Double
darkred = Names.darkred

-- | @#E9967A@, @rgb(233, 150, 122)@, @hsl(15, 72%, 70%)@
darksalmon :: Colour Double
darksalmon = Names.darksalmon

-- | @#8FBC8F@, @rgb(143, 188, 143)@, @hsl(120, 25%, 65%)@
darkseagreen :: Colour Double
darkseagreen = Names.darkseagreen

-- | @#483D8B@, @rgb(72, 61, 139)@, @hsl(248, 39%, 39%)@
darkslateblue :: Colour Double
darkslateblue = Names.darkslateblue

-- | @#2F4F4F@, @rgb(47, 79, 79)@, @hsl(180, 25%, 25%)@. Same as 'darkslategrey'.
darkslategray :: Colour Double
darkslategray = Names.darkslategray

-- | @#2F4F4F@, @rgb(47, 79, 79)@, @hsl(180, 25%, 25%)@. Same as 'darkslategray'.
darkslategrey :: Colour Double
darkslategrey = Names.darkslategrey

-- | @#00CED1@, @rgb(0, 206, 209)@, @hsl(181, 100%, 41%)@
darkturquoise :: Colour Double
darkturquoise = Names.darkturquoise

-- | @#9400D3@, @rgb(148, 0, 211)@, @hsl(282, 100%, 41%)@
darkviolet :: Colour Double
darkviolet = Names.darkviolet

-- | @#FF1493@, @rgb(255, 20, 147)@, @hsl(328, 100%, 54%)@
deeppink :: Colour Double
deeppink = Names.deeppink

-- | @#00BFFF@, @rgb(0, 191, 255)@, @hsl(195, 100%, 50%)@
deepskyblue :: Colour Double
deepskyblue = Names.deepskyblue

-- | @#696969@, @rgb(105, 105, 105)@, @hsl(0, 0%, 41%)@. Same as 'darkgrey'.
dimgray :: Colour Double
dimgray = Names.dimgray

-- | @#696969@, @rgb(105, 105, 105)@, @hsl(0, 0%, 41%)@. Same as 'darkgray'.
dimgrey :: Colour Double
dimgrey = Names.dimgrey

-- | @#1E90FF@, @rgb(30, 144, 255)@, @hsl(210, 100%, 56%)@
dodgerblue :: Colour Double
dodgerblue = Names.dodgerblue

-- | @#B22222@, @rgb(178, 34, 34)@, @hsl(0, 68%, 42%)@
firebrick :: Colour Double
firebrick = Names.firebrick

-- | @#FFFAF0@, @rgb(255, 250, 240)@, @hsl(40, 100%, 97%)@
floralwhite :: Colour Double
floralwhite = Names.floralwhite

-- | @#228B22@, @rgb(34, 139, 34)@, @hsl(120, 61%, 34%)@
forestgreen :: Colour Double
forestgreen = Names.forestgreen

-- | @#FF00FF@, @rgb(255, 0, 255)@, @hsl(300, 100%, 50%)@. Same as 'magenta'.
fuchsia :: Colour Double
fuchsia = Names.fuchsia

-- | @#DCDCDC@, @rgb(220, 220, 220)@, @hsl(0, 0%, 86%)@
gainsboro :: Colour Double
gainsboro = Names.gainsboro

-- | @#F8F8FF@, @rgb(248, 248, 255)@, @hsl(240, 100%, 99%)@
ghostwhite :: Colour Double
ghostwhite = Names.ghostwhite

-- | @#FFD700@, @rgb(255, 215, 0)@, @hsl(51, 100%, 50%)@
gold :: Colour Double
gold = Names.gold

-- | @#DAA520@, @rgb(218, 165, 32)@, @hsl(43, 74%, 49%)@
goldenrod :: Colour Double
goldenrod = Names.goldenrod

-- | @#808080@, @rgb(128, 128, 128)@, @hsl(0, 0%, 50%)@. Same as 'grey'.
gray :: Colour Double
gray = Names.gray

-- | @#008000@, @rgb(0, 128, 0)@, @hsl(120, 100%, 25%)@
green :: Colour Double
green = Names.green

-- | @#808080@, @rgb(128, 128, 128)@, @hsl(0, 0%, 50%)@. Same as 'gray'.
grey :: Colour Double
grey = Names.grey

-- | @#ADFF2F@, @rgb(173, 255, 47)@, @hsl(84, 100%, 59%)@
greenyellow :: Colour Double
greenyellow = Names.greenyellow

-- | @#F0FFF0@, @rgb(240, 255, 240)@, @hsl(120, 100%, 97%)@
honeydew :: Colour Double
honeydew = Names.honeydew

-- | @#FF69B4@, @rgb(255, 105, 180)@, @hsl(330, 100%, 71%)@
hotpink :: Colour Double
hotpink = Names.hotpink

-- | @#CD5C5C@, @rgb(205, 92, 92)@, @hsl(0, 53%, 58%)@
indianred :: Colour Double
indianred = Names.indianred

-- | @#4B0082@, @rgb(75, 0, 130)@, @hsl(275, 100%, 25%)@
indigo :: Colour Double
indigo = Names.indigo

-- | @#FFFFF0@, @rgb(255, 255, 240)@, @hsl(60, 100%, 97%)@
ivory :: Colour Double
ivory = Names.ivory

-- | @#F0E68C@, @rgb(240, 230, 140)@, @hsl(54, 77%, 75%)@
khaki :: Colour Double
khaki = Names.khaki

-- | @#E6E6FA@, @rgb(230, 230, 250)@, @hsl(240, 67%, 94%)@
lavender :: Colour Double
lavender = Names.lavender

-- | @#FFF0F5@, @rgb(255, 240, 245)@, @hsl(340, 100%, 97%)@
lavenderblush :: Colour Double
lavenderblush = Names.lavenderblush

-- | @#7CFC00@, @rgb(124, 252, 0)@, @hsl(90, 100%, 49%)@
lawngreen :: Colour Double
lawngreen = Names.lawngreen

-- | @#FFFACD@, @rgb(255, 250, 205)@, @hsl(54, 100%, 90%)@
lemonchiffon :: Colour Double
lemonchiffon = Names.lemonchiffon

-- | @#ADD8E6@, @rgb(173, 216, 230)@, @hsl(195, 53%, 79%)@
lightblue :: Colour Double
lightblue = Names.lightblue

-- | @#F08080@, @rgb(240, 128, 128)@, @hsl(0, 79%, 72%)@
lightcoral :: Colour Double
lightcoral = Names.lightcoral

-- | @#E0FFFF@, @rgb(224, 255, 255)@, @hsl(180, 100%, 94%)@
lightcyan :: Colour Double
lightcyan = Names.lightcyan

-- | @#FAFAD2@, @rgb(250, 250, 210)@, @hsl(60, 80%, 90%)@
lightgoldenrodyellow :: Colour Double
lightgoldenrodyellow = Names.lightgoldenrodyellow

-- | @#D3D3D3@, @rgb(211, 211, 211)@, @hsl(0, 0%, 83%)@. Same as 'lightgrey'.
lightgray :: Colour Double
lightgray = Names.lightgray

-- | @#90EE90@, @rgb(144, 238, 144)@, @hsl(120, 73%, 75%)@
lightgreen :: Colour Double
lightgreen = Names.lightgreen

-- | @#D3D3D3@, @rgb(211, 211, 211)@, @hsl(0, 0%, 83%)@. Same as 'lightgray'.
lightgrey :: Colour Double
lightgrey = Names.lightgrey

-- | @#FFB6C1@, @rgb(255, 182, 193)@, @hsl(351, 100%, 86%)@
lightpink :: Colour Double
lightpink = Names.lightpink

-- | @#FFA07A@, @rgb(255, 160, 122)@, @hsl(17, 100%, 74%)@
lightsalmon :: Colour Double
lightsalmon = Names.lightsalmon

-- | @#20B2AA@, @rgb(32, 178, 170)@, @hsl(177, 70%, 41%)@
lightseagreen :: Colour Double
lightseagreen = Names.lightseagreen

-- | @#87CEFA@, @rgb(135, 206, 250)@, @hsl(203, 92%, 75%)@
lightskyblue :: Colour Double
lightskyblue = Names.lightskyblue

-- | @#778899@, @rgb(119, 136, 153)@, @hsl(210, 14%, 53%)@. Same as 'lightslategrey'.
lightslategray :: Colour Double
lightslategray = Names.lightslategray

-- | @#778899@, @rgb(119, 136, 153)@, @hsl(210, 14%, 53%)@. Same as 'lightslategray'.
lightslategrey :: Colour Double
lightslategrey = Names.lightslategrey

-- | @#B0C4DE@, @rgb(176, 196, 222)@, @hsl(214, 41%, 78%)@
lightsteelblue :: Colour Double
lightsteelblue = Names.lightsteelblue

-- | @#FFFFE0@, @rgb(255, 255, 224)@, @hsl(60, 100%, 94%)@
lightyellow :: Colour Double
lightyellow = Names.lightyellow

-- | @#00FF00@, @rgb(0, 255, 0)@, @hsl(120, 100%, 50%)@
lime :: Colour Double
lime = Names.lime

-- | @#32CD32@, @rgb(50, 205, 50)@, @hsl(120, 61%, 50%)@
limegreen :: Colour Double
limegreen = Names.limegreen

-- | @#FAF0E6@, @rgb(250, 240, 230)@, @hsl(30, 67%, 94%)@
linen :: Colour Double
linen = Names.linen

-- | @#FF00FF@, @rgb(255, 0, 255)@, @hsl(300, 100%, 50%)@. Same as 'fuchsia'.
magenta :: Colour Double
magenta = Names.magenta

-- | @#800000@, @rgb(128, 0, 0)@, @hsl(0, 100%, 25%)@
maroon :: Colour Double
maroon = Names.maroon

-- | @#66CDAA@, @rgb(102, 205, 170)@, @hsl(160, 51%, 60%)@
mediumaquamarine :: Colour Double
mediumaquamarine = Names.mediumaquamarine

-- | @#0000CD@, @rgb(0, 0, 205)@, @hsl(240, 100%, 40%)@
mediumblue :: Colour Double
mediumblue = Names.mediumblue

-- | @#BA55D3@, @rgb(186, 85, 211)@, @hsl(288, 59%, 58%)@
mediumorchid :: Colour Double
mediumorchid = Names.mediumorchid

-- | @#9370DB@, @rgb(147, 112, 219)@, @hsl(260, 60%, 65%)@
mediumpurple :: Colour Double
mediumpurple = Names.mediumpurple

-- | @#3CB371@, @rgb(60, 179, 113)@, @hsl(147, 50%, 47%)@
mediumseagreen :: Colour Double
mediumseagreen = Names.mediumseagreen

-- | @#7B68EE@, @rgb(123, 104, 238)@, @hsl(249, 80%, 67%)@
mediumslateblue :: Colour Double
mediumslateblue = Names.mediumslateblue

-- | @#00FA9A@, @rgb(0, 250, 154)@, @hsl(157, 100%, 49%)@
mediumspringgreen :: Colour Double
mediumspringgreen = Names.mediumspringgreen

-- | @#48D1CC@, @rgb(72, 209, 204)@, @hsl(178, 60%, 55%)@
mediumturquoise :: Colour Double
mediumturquoise = Names.turquoise

-- | @#C71585@, @rgb(199, 21, 133)@, @hsl(322, 81%, 43%)@
mediumvioletred :: Colour Double
mediumvioletred = Names.mediumvioletred

-- | @#191970@, @rgb(25, 25, 112)@, @hsl(240, 64%, 27%)@
midnightblue :: Colour Double
midnightblue = Names.midnightblue

-- | @#F5FFFA@, @rgb(245, 255, 250)@, @hsl(150, 100%, 98%)@
mintcream :: Colour Double
mintcream = Names.mintcream

-- | @#FFE4E1@, @rgb(255, 228, 225)@, @hsl(6, 100%, 94%)@
mistyrose :: Colour Double
mistyrose = Names.mistyrose

-- | @#FFE4B5@, @rgb(255, 228, 181)@, @hsl(38, 100%, 85%)@
moccasin :: Colour Double
moccasin = Names.moccasin

-- | @#FFDEAD@, @rgb(255, 222, 173)@, @hsl(36, 100%, 84%)@
navajowhite :: Colour Double
navajowhite = Names.navajowhite

-- | @#000080@, @rgb(0, 0, 128)@, @hsl(240, 100%, 25%)@
navy :: Colour Double
navy = Names.navy

-- | @#FDF5E6@, @rgb(253, 245, 230)@, @hsl(39, 85%, 95%)@
oldlace :: Colour Double
oldlace = Names.oldlace

-- | @#808000@, @rgb(128, 128, 0)@, @hsl(60, 100%, 25%)@
olive :: Colour Double
olive = Names.olive

-- | @#6B8E23@, @rgb(107, 142, 35)@, @hsl(80, 60%, 35%)@
olivedrab :: Colour Double
olivedrab = Names.olivedrab

-- | @#FFA500@, @rgb(255, 165, 0)@, @hsl(39, 100%, 50%)@
orange :: Colour Double
orange = Names.orange

-- | @#FF4500@, @rgb(255, 69, 0)@, @hsl(16, 100%, 50%)@
orangered :: Colour Double
orangered = Names.orangered

-- | @#DA70D6@, @rgb(218, 112, 214)@, @hsl(302, 59%, 65%)@
orchid :: Colour Double
orchid = Names.orchid

-- | @#EEE8AA@, @rgb(238, 232, 170)@, @hsl(55, 67%, 80%)@
palegoldenrod :: Colour Double
palegoldenrod = Names.palegoldenrod

-- | @#98FB98@, @rgb(152, 251, 152)@, @hsl(120, 93%, 79%)@
palegreen :: Colour Double
palegreen = Names.palegreen

-- | @#AFEEEE@, @rgb(175, 238, 238)@, @hsl(180, 65%, 81%)@
paleturquoise :: Colour Double
paleturquoise = Names.paleturquoise

-- | @#DB7093@, @rgb(219, 112, 147)@, @hsl(340, 60%, 65%)@
palevioletred :: Colour Double
palevioletred = Names.palevioletred

-- | @#FFEFD5@, @rgb(255, 239, 213)@, @hsl(37, 100%, 92%)@
papayawhip :: Colour Double
papayawhip = Names.papayawhip

-- | @#FFDAB9@, @rgb(255, 218, 185)@, @hsl(28, 100%, 86%)@
peachpuff :: Colour Double
peachpuff = Names.peachpuff

-- | @#CD853F@, @rgb(205, 133, 63)@, @hsl(30, 59%, 53%)@
peru :: Colour Double
peru = Names.peru

-- | @#FFC0CB@, @rgb(255, 192, 203)@, @hsl(350, 100%, 88%)@
pink :: Colour Double
pink = Names.pink

-- | @#DDA0DD@, @rgb(221, 160, 221)@, @hsl(300, 47%, 75%)@
plum :: Colour Double
plum = Names.plum

-- | @#B0E0E6@, @rgb(176, 224, 230)@, @hsl(187, 52%, 80%)@
powderblue :: Colour Double
powderblue = Names.powderblue

-- | @#800080@, @rgb(128, 0, 128)@, @hsl(300, 100%, 25%)@
purple :: Colour Double
purple = Names.purple

-- | @#FF0000@, @rgb(255, 0, 0)@, @hsl(0, 100%, 50%)@
red :: Colour Double
red = Names.red

-- | @#BC8F8F@, @rgb(188, 143, 143)@, @hsl(0, 25%, 65%)@
rosybrown :: Colour Double
rosybrown = Names.rosybrown

-- | @#4169E1@, @rgb(65, 105, 225)@, @hsl(225, 73%, 57%)@
royalblue :: Colour Double
royalblue = Names.royalblue

-- | @#8B4513@, @rgb(139, 69, 19)@, @hsl(25, 76%, 31%)@
saddlebrown :: Colour Double
saddlebrown = Names.saddlebrown

-- | @#FA8072@, @rgb(250, 128, 114)@, @hsl(6, 93%, 71%)@
salmon :: Colour Double
salmon = Names.salmon

-- | @#F4A460@, @rgb(244, 164, 96)@, @hsl(28, 87%, 67%)@
sandybrown :: Colour Double
sandybrown = Names.sandybrown

-- | @#2E8B57@, @rgb(46, 139, 87)@, @hsl(146, 50%, 36%)@
seagreen :: Colour Double
seagreen = Names.seagreen

-- | @#FFF5EE@, @rgb(255, 245, 238)@, @hsl(25, 100%, 97%)@
seashell :: Colour Double
seashell = Names.seashell

-- | @#A0522D@, @rgb(160, 82, 45)@, @hsl(19, 56%, 40%)@
sienna :: Colour Double
sienna = Names.sienna

-- | @#C0C0C0@, @rgb(192, 192, 192)@, @hsl(0, 0%, 75%)@
silver :: Colour Double
silver = Names.silver

-- | @#87CEEB@, @rgb(135, 206, 235)@, @hsl(197, 71%, 73%)@
skyblue :: Colour Double
skyblue = Names.skyblue

-- | @#6A5ACD@, @rgb(106, 90, 205)@, @hsl(248, 53%, 58%)@
slateblue :: Colour Double
slateblue = Names.slateblue

-- | @#708090@, @rgb(112, 128, 144)@, @hsl(210, 13%, 50%)@. Same as 'slategrey'.
slategray :: Colour Double
slategray = Names.slategray

-- | @#708090@, @rgb(112, 128, 144)@, @hsl(210, 13%, 50%)@. Same as 'slategray'.
slategrey :: Colour Double
slategrey = Names.slategrey

-- | @#FFFAFA@, @rgb(255, 250, 250)@, @hsl(0, 100%, 99%)@
snow :: Colour Double
snow = Names.snow

-- | @#00FF7F@, @rgb(0, 255, 127)@, @hsl(150, 100%, 50%)@
springgreen :: Colour Double
springgreen = Names.springgreen

-- | @#4682B4@, @rgb(70, 130, 180)@, @hsl(207, 44%, 49%)@
steelblue :: Colour Double
steelblue = Names.steelblue

-- | @#D2B48C@, @rgb(210, 180, 140)@, @hsl(34, 44%, 69%)@
tan :: Colour Double
tan = Names.tan

-- | @#008080@, @rgb(0, 128, 128)@, @hsl(180, 100%, 25%)@
teal :: Colour Double
teal = Names.teal

-- | @#D8BFD8@, @rgb(216, 191, 216)@, @hsl(300, 24%, 80%)@
thistle :: Colour Double
thistle = Names.thistle

-- | @#FF6347@, @rgb(255, 99, 71)@, @hsl(9, 100%, 64%)@
tomato :: Colour Double
tomato = Names.tomato

-- | @#40E0D0@, @rgb(64, 224, 208)@, @hsl(174, 72%, 56%)@
turquoise :: Colour Double
turquoise = Names.turquoise

-- | @#EE82EE@, @rgb(238, 130, 238)@, @hsl(300, 76%, 72%)@
violet :: Colour Double
violet = Names.violet

-- | @#F5DEB3@, @rgb(245, 222, 179)@, @hsl(39, 77%, 83%)@
wheat :: Colour Double
wheat = Names.wheat

-- | @#FFFFFF@, @rgb(255, 255, 255)@, @hsl(0, 100%, 100%)@
white :: Colour Double
white = Names.white

-- | @#F5F5F5@, @rgb(245, 245, 245)@, @hsl(0, 0%, 96%)@
whitesmoke :: Colour Double
whitesmoke = Names.whitesmoke

-- | @#FFFF00@, @rgb(255, 255, 0)@, @hsl(60, 100%, 50%)@
yellow :: Colour Double
yellow = Names.yellow

-- | @#9ACD32@, @rgb(154, 205, 50)@, @hsl(80, 61%, 50%)@
yellowgreen :: Colour Double
yellowgreen = Names.yellowgreen

-- | @#663399@, @rgb(102, 51, 153)@, @hsl(270, 50%, 40%)@
rebeccapurple :: Colour Double
rebeccapurple = sRGB24 102 51 153

-- |
-- This 'AlphaColour' is entirely transparent and has no associated
-- color channel (i.e., @rgba(0, 0, 0, 0.0)@ or @hsla(0, 0%, 0%, 0.0)@).
transparent :: AlphaColour Double
transparent = Colour.transparent
