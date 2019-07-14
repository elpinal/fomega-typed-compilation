{-# LANGUAGE ExistentialQuantification #-}

module Target.Term
  ( LSym(..)
  , Symantics(..)

  , module Target.Type
  ) where

import qualified Facade.Term as F
import Target.Type

class LSym repr where
  int :: repr Int
  bool :: repr Bool
  string :: repr String
  unit :: repr ()

class LSym repr => Symantics repr where
  abs_ :: (repr a -> repr b) -> repr (a -> b)
  app :: repr (a -> b) -> repr a -> repr b
  pr :: repr String -> repr ()
  int2string :: repr Int -> repr String
  bool2string :: repr Bool -> repr String
  sub :: repr Int -> repr Int -> repr Int
  if_ :: repr Bool -> repr a -> repr a -> repr a
  concat_ :: repr String -> repr String -> repr String

data DynTerm repr = forall t. DynTerm (repr t)

from :: F.Term -> DynTerm repr
from = undefined
