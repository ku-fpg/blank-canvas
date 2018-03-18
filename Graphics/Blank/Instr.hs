{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}

module Graphics.Blank.Instr
  (Instr
  ,surround
  ,surroundIf
  ,singleton
  ,showiUnaryWith
  ,showiHex
  ,fromBuilder
  ,toBuilder
  ,toString
  ,fromText
  ,toLazyText
  ,InstrShow(..)
  ,ContextShow(..)
  ,tshowi
  ,tshowiPrec
  )
  where

import           Data.Text.Lazy hiding (singleton)

import           Data.Text.Lazy.Builder hiding (fromString, singleton, fromText, toLazyText)
import qualified Data.Text.Lazy.Builder as T
import           TextShow hiding (fromString, singleton, fromText, toLazyText, toString)
import qualified TextShow as TS

import           Data.Semigroup
import           Data.String

import           GHC.Show (appPrec, appPrec1)
import           Numeric (showHex)

import           Data.Word

data Instr = Instr Builder Builder

fromBuilder :: Builder -> Instr
fromBuilder a = Instr a mempty

fromText :: Text -> Instr
fromText a = Instr (T.fromLazyText a) mempty

surround :: Builder -> Builder -> Instr
surround = Instr

surroundIf :: Bool -> Builder -> Builder -> Instr
surroundIf True  a b = surround a b
surroundIf False _ _ = mempty

toBuilder :: Instr -> Builder
toBuilder (Instr a b) = a <> b

toLazyText :: Instr -> Text
toLazyText = T.toLazyText . toBuilder

toString :: Instr -> String
toString = TS.toString . toBuilder

singleton :: Char -> Instr
singleton = fromBuilder . T.singleton

showiUnaryWith :: (Int -> a -> Instr) -> Instr -> Int -> a -> Instr
showiUnaryWith sp nameB p x =
  surroundIf (p > appPrec) "(" ")" <>
    nameB <> singleton ' ' <> sp appPrec1 x

showiHex :: (Integral a, Show a) => a -> Instr
showiHex i = fromString $ showHex i ""

instance Semigroup Instr where
    (<>) = mappend

instance Monoid Instr where
    mempty = Instr mempty mempty
    mappend (Instr a1 b1)
            (Instr a2 b2) =
      Instr (a1 <> a2)
            (b2 <> b1)

instance IsString Instr where
    fromString = fromBuilder . fromString

instance TextShow Instr where
    showb = toBuilder

class InstrShow a where
    showi :: a -> Instr
    showiPrec :: Int -> a -> Instr

    default showi :: TextShow a => a -> Instr
    showi = fromBuilder . showb

    default showiPrec :: TextShow a => Int -> a -> Instr
    showiPrec p = fromBuilder . showbPrec p

class ContextShow a where
    showc :: a -> Instr -> Instr

instance InstrShow Instr where
    showi = id
    showiPrec _ = id

instance InstrShow Word8
instance InstrShow Double
instance InstrShow Int
instance InstrShow Text
instance InstrShow Bool
instance InstrShow ()

tshowi :: TextShow a => a -> Instr
tshowi = fromBuilder . showb

tshowiPrec :: TextShow a => Int -> a -> Instr
tshowiPrec p = fromBuilder . showbPrec p

