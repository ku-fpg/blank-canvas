{-# LANGUAGE OverloadedStrings #-}
module Graphics.Blank.Types.Cursor where

import           Data.Monoid
import           Data.String (IsString(..))
import qualified Data.Text as TS (Text)
import           Data.Text (pack)

import           Graphics.Blank.JavaScript
import           Graphics.Blank.Parser (stringCI, unlift)

import           Prelude ()
import           Prelude.Compat

import           Text.ParserCombinators.ReadP (ReadP, (<++), between, char,
                                               choice, munch, skipSpaces)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..), readListPrecDefault)
import qualified Text.Show      as S (Show)
import qualified Text.Show.Text as T (Show)
import           Text.Show.Text hiding (Show)

-- | A data type that can represent a browser cursor.
class CanvasCursor a where
    -- | Convert a value into a JavaScript string representing a cursor value.
    jsCanvasCursor :: a -> Builder

instance CanvasCursor TS.Text where
    jsCanvasCursor = jsText

instance CanvasCursor Cursor where
    jsCanvasCursor = jsCursor

-- | Specified the mouse cursor's appearance in a web browser.
-- 
-- Images by the Mozilla Developer Network are licensed under
-- <http://creativecommons.org/licenses/by-sa/2.5/ CC-BY-SA 2.5>.
data Cursor = Auto         -- ^ The browser determines the cursor to display based on the
                           --   current context.
            | Default      -- ^ <<https://developer.mozilla.org/@api/deki/files/3438/=default.gif>>
            | None         -- ^ No cursor is rendered.
            | ContextMenu  -- ^ <<https://developer.mozilla.org/@api/deki/files/3461/=context-menu.png>>
            | Help         -- ^ <<https://developer.mozilla.org/@api/deki/files/3442/=help.gif>>
            | Pointer      -- ^ <<https://developer.mozilla.org/@api/deki/files/3449/=pointer.gif>>
            | Progress     -- ^ <<https://developer.mozilla.org/@api/deki/files/3450/=progress.gif>>
            | Wait         -- ^ <<https://developer.mozilla.org/@api/deki/files/3457/=wait.gif>>
            | Cell         -- ^ <<https://developer.mozilla.org/@api/deki/files/3434/=cell.gif>>
            | Crosshair    -- ^ <<https://developer.mozilla.org/@api/deki/files/3437/=crosshair.gif>>
            | Text         -- ^ <<https://developer.mozilla.org/files/3809/text.gif>>
            | VerticalText -- ^ <<https://developer.mozilla.org/@api/deki/files/3456/=vertical-text.gif>>
            | Alias        -- ^ <<https://developer.mozilla.org/@api/deki/files/3432/=alias.gif>>
            | Copy         -- ^ <<https://developer.mozilla.org/@api/deki/files/3436/=copy.gif>>
            | Move         -- ^ <<https://developer.mozilla.org/@api/deki/files/3443/=move.gif>>
            | NoDrop       -- ^ <<https://developer.mozilla.org/@api/deki/files/3445/=no-drop.gif>>
            | NotAllowed   -- ^ <<https://developer.mozilla.org/@api/deki/files/3446/=not-allowed.gif>>
            | AllScroll    -- ^ <<https://developer.mozilla.org/@api/deki/files/3433/=all-scroll.gif>>
            | ColResize    -- ^ <<https://developer.mozilla.org/@api/deki/files/3435/=col-resize.gif>>
            | RowResize    -- ^ <<https://developer.mozilla.org/@api/deki/files/3451/=row-resize.gif>>
            | NResize      -- ^ <<https://developer.mozilla.org/files/4083/n-resize.gif>>
            | EResize      -- ^ <<https://developer.mozilla.org/files/4085/e-resize.gif>>
            | SResize      -- ^ <<https://developer.mozilla.org/files/4087/s-resize.gif>>
            | WResize      -- ^ <<https://developer.mozilla.org/files/4089/w-resize.gif>>
            | NEResize     -- ^ <<https://developer.mozilla.org/files/4091/ne-resize.gif>>
            | NWResize     -- ^ <<https://developer.mozilla.org/files/4093/nw-resize.gif>>
            | SEResize     -- ^ <<https://developer.mozilla.org/files/4097/se-resize.gif>>
            | SWResize     -- ^ <<https://developer.mozilla.org/files/4095/sw-resize.gif>>
            | EWResize     -- ^ <<https://developer.mozilla.org/files/3806/3-resize.gif>>
            | NSResize     -- ^ <<https://developer.mozilla.org/files/3808/6-resize.gif>>
            | NESWResize   -- ^ <<https://developer.mozilla.org/files/3805/1-resize.gif>>
            | NWSEResize   -- ^ <<https://developer.mozilla.org/files/3807/4-resize.gif>>
            | ZoomIn       -- ^ <<https://developer.mozilla.org/@api/deki/files/3459/=zoom-in.gif>>
            | ZoomOut      -- ^ <<https://developer.mozilla.org/@api/deki/files/3460/=zoom-out.gif>>
            | Grab         -- ^ <<https://developer.mozilla.org/@api/deki/files/3440/=grab.gif>>
            | Grabbing     -- ^ <<https://developer.mozilla.org/@api/deki/files/3441/=grabbing.gif>>
            | URL TS.Text Cursor
              -- ^ An image from a URL. Must be followed by another 'Cursor'.
    deriving (Eq, Ord)

instance IsString Cursor where
    fromString = read

instance JSArg Cursor where
    showbJS = jsCursor

jsCursor :: Cursor -> Builder
jsCursor = jsLiteralBuilder . showb

instance Read Cursor where
    readPrec = lift $ do
        skipSpaces
        choice
          [ Auto         <$ stringCI "auto"
          , Default      <$ stringCI "default"
          , None         <$ stringCI "none"
          , ContextMenu  <$ stringCI "context-menu"
          , Help         <$ stringCI "help"
          , Pointer      <$ stringCI "pointer"
          , Progress     <$ stringCI "progress"
          , Wait         <$ stringCI "wait"
          , Cell         <$ stringCI "cell"
          , Crosshair    <$ stringCI "crosshair"
          , Text         <$ stringCI "text"
          , VerticalText <$ stringCI "vertical-text"
          , Alias        <$ stringCI "alias"
          , Copy         <$ stringCI "copy"
          , Move         <$ stringCI "move"
          , NoDrop       <$ stringCI "no-drop"
          , NotAllowed   <$ stringCI "not-allowed"
          , AllScroll    <$ stringCI "all-scroll"
          , ColResize    <$ stringCI "col-resize"
          , RowResize    <$ stringCI "row-resize"
          , NResize      <$ stringCI "n-resize"
          , EResize      <$ stringCI "e-resize"
          , SResize      <$ stringCI "s-resize"
          , WResize      <$ stringCI "w-resize"
          , NEResize     <$ stringCI "ne-resize"
          , NWResize     <$ stringCI "nw-resize"
          , SEResize     <$ stringCI "se-resize"
          , SWResize     <$ stringCI "sw-resize"
          , EWResize     <$ stringCI "ew-resize"
          , NSResize     <$ stringCI "ns-resize"
          , NESWResize   <$ stringCI "nesw-resize"
          , NWSEResize   <$ stringCI "nwse-resize"
          , ZoomIn       <$ stringCI "zoom-in"
          , ZoomOut      <$ stringCI "zoom-out"
          , Grab         <$ stringCI "grab"
          , Grabbing     <$ stringCI "grabbing"
          , do _ <- stringCI "url("
               let quoted quote = between (char quote) (char quote)
               url' <- quoted '"' (readURL $ Just '"')
                 <++ quoted '\'' (readURL $ Just '\'')
                 <++ readURL Nothing
               _ <- char ')'
               skipSpaces
               _ <- char ','
               URL url' <$> unlift readPrec
          ]
    
    readListPrec = readListPrecDefault

readURL :: Maybe Char -> ReadP TS.Text
readURL mQuote = do
    url' <- case mQuote of
        Just quote -> munch (/= quote)
        Nothing    -> munch (/= ')')
    return $ pack url'

instance S.Show Cursor where
    showsPrec p = showsPrec p . FromTextShow

instance T.Show Cursor where
    showb Auto         = "auto"
    showb Default      = "default"
    showb None         = "none"
    showb ContextMenu  = "context-menu"
    showb Help         = "help"
    showb Pointer      = "pointer"
    showb Progress     = "progress"
    showb Wait         = "wait"
    showb Cell         = "cell"
    showb Crosshair    = "crosshair"
    showb Text         = "text"
    showb VerticalText = "vertical-text"
    showb Alias        = "alias"
    showb Copy         = "copy"
    showb Move         = "move"
    showb NoDrop       = "no-drop"
    showb NotAllowed   = "not-allowed"
    showb AllScroll    = "all-scroll"
    showb ColResize    = "col-resize"
    showb RowResize    = "row-resize"
    showb NResize      = "n-resize"
    showb EResize      = "e-resize"
    showb SResize      = "s-resize"
    showb WResize      = "w-resize"
    showb NEResize     = "ne-resize"
    showb NWResize     = "nw-resize"
    showb SEResize     = "se-resize"
    showb SWResize     = "sw-resize"
    showb EWResize     = "ew-resize"
    showb NSResize     = "ns-resize"
    showb NESWResize   = "nesw-resize"
    showb NWSEResize   = "nwse-resize"
    showb ZoomIn       = "zoom-in"
    showb ZoomOut      = "zoom-out"
    showb Grab         = "grab"
    showb Grabbing     = "grabbing"
    showb (URL url' cur) =
        "url(" <> jsLiteralBuilder (fromText url') <> "), " <> showb cur

-- | Shorthand for 'Auto'.
auto :: Cursor
auto = Auto

-- | Shorthand for 'Default', with an underscore to distinguish it from the
-- Haskell keyword @default@.
default_ :: Cursor
default_ = Default

-- | Shorthand for 'None'.
none :: Cursor
none = None

-- | Shorthand for 'ContextMenu'.
contextMenu :: Cursor
contextMenu = ContextMenu

-- | Shorthand for 'Help'.
help :: Cursor
help = Help

-- | Shorthand for 'Pointer'.
pointer :: Cursor
pointer = Pointer

-- | Shorthand for 'Progress'.
progress :: Cursor
progress = Progress

-- | Shorthand for 'Wait'.
wait :: Cursor
wait = Wait

-- | Shorthand for 'Cell'.
cell :: Cursor
cell = Cell

-- | Shorthand for 'Crosshair'.
crosshair :: Cursor
crosshair = Crosshair

-- | Shorthand for 'Text'.
text :: Cursor
text = Text

-- | Shorthand for 'VerticalText'.
verticalText :: Cursor
verticalText = VerticalText

-- | Shorthand for 'Alias'.
alias :: Cursor
alias = Alias

-- | Shorthand for 'Copy'.
copy :: Cursor
copy = Copy

-- | Shorthand for 'Move'.
move :: Cursor
move = Move

-- | Shorthand for 'NoDrop'.
noDrop :: Cursor
noDrop = NoDrop

-- | Shorthand for 'NotAllowed'.
notAllowed :: Cursor
notAllowed = NotAllowed

-- | Shorthand for 'AllScroll'.
allScroll :: Cursor
allScroll = AllScroll

-- | Shorthand for 'ColResize'.
colResize :: Cursor
colResize = ColResize

-- | Shorthand for 'RowResize'.
rowResize :: Cursor
rowResize = RowResize

-- | Shorthand for 'NResize'.
nResize :: Cursor
nResize = NResize

-- | Shorthand for 'EResize'.
eResize :: Cursor
eResize = EResize

-- | Shorthand for 'SResize'.
sResize :: Cursor
sResize = SResize

-- | Shorthand for 'WResize'.
wResize :: Cursor
wResize = WResize

-- | Shorthand for 'NEResize'.
neResize :: Cursor
neResize = NEResize

-- | Shorthand for 'NWResize'.
nwResize :: Cursor
nwResize = NWResize

-- | Shorthand for 'SEResize'.
seResize :: Cursor
seResize = SEResize

-- | Shorthand for 'SWResize'.
swResize :: Cursor
swResize = SWResize

-- | Shorthand for 'EWResize'.
ewResize :: Cursor
ewResize = ewResize

-- | Shorthand for 'NSResize'.
nsResize :: Cursor
nsResize = NSResize

-- | Shorthand for 'NESWResize'.
neswResize :: Cursor
neswResize = NESWResize

-- | Shorthand for 'NWSEResize'.
nwseResize :: Cursor
nwseResize = NWSEResize

-- | Shorthand for 'ZoomIn'.
zoomIn :: Cursor
zoomIn = ZoomIn

-- | Shorthand for 'ZoomOut'.
zoomOut :: Cursor
zoomOut = ZoomOut

-- | Shorthand for 'Grab'.
grab :: Cursor
grab = Grab

-- | Shorthand for 'Grabbing'.
grabbing :: Cursor
grabbing = Grabbing

-- | Shorthand for 'URL'.
url :: TS.Text -> Cursor -> Cursor
url = URL
