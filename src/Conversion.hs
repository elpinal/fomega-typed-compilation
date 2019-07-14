{-# LANGUAGE MultiParamTypeClasses #-}

module Conversion
  ( From(..)
  ) where

class Monad m => From m a b where
  from :: a -> m b
