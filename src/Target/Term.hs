{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Target.Term
  ( LSym(..)
  , Symantics(..)

  , module Target.Type
  ) where

import Conversion
import qualified Facade.Term as F
import Facade.Term (Literal(..), Term(..))
import Target.Type

class LSym repr where
  int :: Int -> repr Int
  bool :: Bool -> repr Bool
  string :: String -> repr String
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

data DynTerm repr = forall t. DynTerm (TQ t) (repr t)

instance (Monad m, LSym repr) => From m Literal (DynTerm repr) where
  from (Int x)    = return $ DynTerm tint $ int x
  from (Bool x)   = return $ DynTerm tbool $ bool x
  from (String x) = return $ DynTerm tstring $ string x
  from Unit       = return $ DynTerm tunit $ unit

instance (Monad m, Symantics repr) => From m Term (DynTerm repr) where
  from (Lit l) = from l
  from (Print t) = do
    DynTerm (TQ ty) x <- from t
    maybe (fail "not string") (return . DynTerm tunit . pr) $ asString ty x
