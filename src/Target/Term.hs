{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Target.Term
  ( LSym(..)
  , Symantics(..)
  , DynTerm(..)

  , module Target.Type
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

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
  abs_ :: repr (a, h) b -> repr h (a -> E b)
  app :: repr h (a -> E b) -> repr h a -> repr h b
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

newtype V t = V (TQ t)

class Monad (m gamma h) => EnvM m gamma h where
  withBinding :: TQ t -> m (V t, gamma) (t, h) a -> m gamma h a
  lookupVar :: Symantics repr => F.Variable -> m gamma h (Maybe (DynTerm repr h))

newtype M gamma h a = M { unM :: gamma -> a }
  deriving (Functor, Applicative, Monad)

instance EnvM M () () where
  withBinding tq (M m) = M $ \gamma -> m (V tq, gamma)
  lookupVar _ = return Nothing

instance EnvM M gamma h => EnvM M (V a, gamma) (a, h) where
  withBinding tq (M m) = M $ \gamma -> m (V tq, gamma)
  lookupVar (F.Variable 0) = do
    (V tq, _) <- M id
    return $ Just $ DynTerm tq vz
  lookupVar (F.Variable n) = do
    x <- M $ \(_, gamma) -> unM (lookupVar (F.Variable $ n - 1)) gamma
    case x of
      Just (DynTerm tq t) -> return $ Just $ DynTerm tq $ vs t
      Nothing             -> return Nothing

instance (EnvM M gamma h, Symantics repr) => From (M gamma h) Term (DynTerm repr h) where
  from (Var v)    = lookupVar v >>= maybe (fail $ "unbound variable: " ++ show v) return
  from (Abs ty t) = do
    Type ty1 <- from ty
    DynTerm tq x <- withBinding ty1 $ from t
    return $ DynTerm (tarrow ty1 tq) $ abs_ x
  from (App t1 t2) = do
    DynTerm ty1 x <- from t1
    DynTerm ty2 y <- from t2
    AsArrow _ m <- return $ getTQ ty1
    (ty11, ty12, eq) <- maybe (fail "not function") return m
    y <- maybe (fail "type mismatch") return $ cast ty2 y ty11
    return $ DynTerm ty12 $ app (getEquality eq x) y
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

convert' :: Symantics repr => Term -> M () () (DynTerm repr ())
convert' = from

convert :: Symantics repr => Term -> DynTerm repr ()
convert = ($ ()) . unM . convert'

newtype R h a = R { unR :: h -> E a }

lit :: a -> R h a
lit = R . const . pure

instance LSym R where
  int x    = lit x
  bool x   = lit x
  string x = lit x
  unit     = lit ()

instance Symantics R where
  vz            = R $ pure . fst
  vs r          = R $ unR r . snd
  abs_ r        = R $ \env -> pure $ \x -> unR r (x, env)
  app r1 r2     = R $ \env -> join $ unR r1 env <*> unR r2 env
  pr r          = R $ \env -> unR r env >>= liftIO . putStrLn
  int2string r  = R $ fmap show . unR r
  bool2string r = R $ fmap show . unR r
  sub r1 r2     = R $ \env -> liftA2 (-) (unR r1 env) $ unR r2 env
  if_ r1 r2 r3  = R $ \env -> do
    b <- unR r1 env
    if b
      then unR r2 env
      else unR r3 env
  concat_ r1 r2 = R $ \env -> liftA2 (<>) (unR r1 env) $ unR r2 env

-- Evaluate a facade term in empty environment.
evalF :: Term -> E ()
evalF t = f $ convert t
  where
    f :: DynTerm R () -> E ()
    f (DynTerm _ r) = void $ unR r ()
