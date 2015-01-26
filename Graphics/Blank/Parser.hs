module Graphics.Blank.Parser where

import Control.Applicative hiding (many, optional)

import Data.CaseInsensitive (mk)
import Data.Char
import Data.Ix
import Data.Functor (void)

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (ReadPrec, readPrec_to_P)

-- | maybeRead p will either parse p or return Nothing without consuming any
--   input. Compare to optional from Text.ParserCombinators.ReadP.
maybeRead :: ReadPrec a -> ReadPrec (Maybe a)
maybeRead p = (Just <$> p) <|> return Nothing

-- | A case-insensitive version of string from Text.ParserCombinators.ReadP.
stringCI :: String -> ReadP String
stringCI this = look >>= scan this
  where
    scan :: String -> String -> ReadP String
    scan []     _                     = return this
    scan (x:xs) (y:ys) | mk x == mk y = get *> scan xs ys
    scan _      _                     = pfail

-- | Convert a ReadPrec to a ReadP (the converse of lift).
unlift :: ReadPrec a -> ReadP a
unlift = flip readPrec_to_P 0

-- | Equivalent to the function from parsec, but using ReadP.
noneOf :: [Char] -> ReadP Char
noneOf cs = satisfy $ \c -> not $ elem c cs

-------------------------------------------------------------------------------
-- Parser combinators for CSS identifiers. Adapted from the hxt-css package.
-------------------------------------------------------------------------------

-- | Parses a CSS identifier.
cssIdent :: ReadP String
cssIdent = (:) <$> nmstart <*> many nmchar

-- | Parses the beginning character of a CSS identifier.
nmstart :: ReadP Char
nmstart = satisfy p <|> nonascii
  where
    p c = inRange ('a', 'z') c || inRange ('A', 'Z') c || c == '_'

-- | Parses a non-beginning CSS identifier character.
nmchar :: ReadP Char
nmchar = satisfy p <|> nonascii
  where
    p c = inRange ('a', 'z') c || inRange ('A', 'Z') c ||
        isDigit c || elem c "_-"

-- | Parses a CSS string literal.
stringLit :: ReadP String
stringLit = string1 <|> string2
  where
    string1 = char '"'
           *> many (noneOf "\n\r\f\\\"" <|> nl <|> nonascii)
           <* char '*'
    string2 = char '\''
           *> many (noneOf "\n\r\f\\'"  <|> nl <|> nonascii)
           <* char '\''

-- | Parses a non-ASCII CSS character.
nonascii :: ReadP Char
nonascii = satisfy (> '\DEL')

-- | Parses a CSS-style newline.
nl :: ReadP Char
nl = choice
    [ void $ char '\n'
    , char '\r' >> optional (char '\n')
    , void $ char '\f'
    ] >> return '\n'
