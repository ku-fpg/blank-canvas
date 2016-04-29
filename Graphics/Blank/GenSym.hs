{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Graphics.Blank.GenSym where

import           Control.Concurrent.STM
import           Control.Monad.Reader

newtype GenSym a = GenSym (ReaderT (TVar Int) STM a)
                   deriving (Functor, Applicative, Monad)

uniq :: GenSym Int
uniq = GenSym $ do
  sym <- ask
  val <- lift $ readTVar sym
  lift $ modifyTVar sym (+1)
  return val

runGenSym :: Int -> GenSym a -> IO a
runGenSym initialVal (GenSym gen) =
  atomically $ do
    sym <- newTVar initialVal
    runReaderT gen sym

