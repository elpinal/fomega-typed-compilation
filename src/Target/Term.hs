{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Target.Term
  ( LSym(..)
  , Symantics(..)
  , DynTerm(..)

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

realize :: TSym (As t) => DynTerm repr -> Maybe (repr t)
realize (DynTerm (TQ ty) x) = as ty x

instance (Monad m, Symantics repr) => From m Term (DynTerm repr) where
  from (Lit l)         = from l
  from (Print t)       = from t >>= maybe (fail "not string") (return . DynTerm tunit . pr) . realize
  from (Int2String t)  = from t >>= maybe (fail "not integer") (return . DynTerm tstring . int2string) . realize
  from (Bool2String t) = from t >>= maybe (fail "not boolean") (return . DynTerm tstring . bool2string) . realize
  from (Sub t1 t2)     = do
    x <- from t1 >>= maybe (fail "not integer") return . realize
    y <- from t2 >>= maybe (fail "not integer") return . realize
    return $ DynTerm tint $ sub x y
  from (If t1 t2 t3) = do
    x <- from t1 >>= maybe (fail "not boolean") return . realize
    DynTerm ty2 y <- from t2
    DynTerm ty3 z <- from t3
    z <- maybe (fail "type mismatch") return $ cast ty3 z ty2
    return $ DynTerm ty2 $ if_ x y z
  from (Concat t1 t2) = do
    x <- from t1 >>= maybe (fail "not string") return . realize
    y <- from t2 >>= maybe (fail "not string") return . realize
    return $ DynTerm tstring $ concat_ x y
