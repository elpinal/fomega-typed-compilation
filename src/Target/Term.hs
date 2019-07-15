{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Target.Term
  ( LSym(..)
  , Symantics(..)
  , DynTerm(..)

  , module Target.Type
  ) where

import Conversion
import qualified Facade.Term as F
import Facade.Term (Literal(..), Term(..), BaseType(..))
import Target.Type

class LSym repr where
  int :: Int -> repr h Int
  bool :: Bool -> repr h Bool
  string :: String -> repr h String
  unit :: repr h ()

class LSym repr => Symantics repr where
  vz :: repr (a, h) a
  vs :: repr h a -> repr (b, h) a
  abs_ :: repr (a, h) b -> repr h (a -> b)
  app :: repr h (a -> b) -> repr h a -> repr h b
  pr :: repr h String -> repr h ()
  int2string :: repr h Int -> repr h String
  bool2string :: repr h Bool -> repr h String
  sub :: repr h Int -> repr h Int -> repr h Int
  if_ :: repr h Bool -> repr h a -> repr h a -> repr h a
  concat_ :: repr h String -> repr h String -> repr h String

data Type = forall t. Type (TQ t)

instance Monad m => From m BaseType Type where
  from TInt    = return $ Type tint
  from TBool   = return $ Type tbool
  from TString = return $ Type tstring
  from TUnit   = return $ Type tunit

instance Monad m => From m F.Type Type where
  from (F.BaseType b)     = from b
  from (F.TArrow ty1 ty2) = do
    Type tq1 <- from ty1
    Type tq2 <- from ty2
    return $ Type $ tarrow tq1 tq2

data DynTerm repr h = forall t. DynTerm (TQ t) (repr h t)

instance (Monad m, LSym repr) => From m Literal (DynTerm repr h) where
  from (Int x)    = return $ DynTerm tint $ int x
  from (Bool x)   = return $ DynTerm tbool $ bool x
  from (String x) = return $ DynTerm tstring $ string x
  from Unit       = return $ DynTerm tunit $ unit

realize :: TSym (As t) => DynTerm repr h -> Maybe (repr h t)
realize (DynTerm (TQ ty) x) = as ty x

class Monad (m h) => EnvM m repr h where
  withBinding :: repr h t -> m (t, h) a -> m h a
  lookupVar :: F.Variable -> m h (Maybe (DynTerm repr h))

instance (EnvM m repr h, Symantics repr) => From (m h) Term (DynTerm repr h) where
  from (Var v)         = lookupVar v >>= maybe (fail $ "unbound variable: " ++ show v) return
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
