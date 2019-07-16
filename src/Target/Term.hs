{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

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
  int :: Int -> repr h i Int
  bool :: Bool -> repr h i Bool
  string :: String -> repr h i String
  unit :: repr h i ()

type Arrow a b = a -> E b

infixr 2 -@>
type a -@> b = Arrow a b

class LSym repr => Symantics repr where
  vz :: repr (a, h) i a
  vs :: repr h i a -> repr (b, h) i a
  abs_ :: repr (a, h) i b -> repr h i (a -@> b)
  app :: repr h i (a -@> b) -> repr h i a -> repr h i b
  pr :: repr h i String -> repr h i ()
  int2string :: repr h i Int -> repr h i String
  bool2string :: repr h i Bool -> repr h i String
  add :: repr h i (Int -@> Int -@> Int)
  sub :: repr h i Int -> repr h i Int -> repr h i Int
  if_ :: repr h i Bool -> repr h i a -> repr h i a -> repr h i a
  concat_ :: repr h i String -> repr h i String -> repr h i String

data Type i = forall t. Type (TQ i t)

instance Monad m => From m BaseType (Type i) where
  from TInt    = return $ Type tint
  from TBool   = return $ Type tbool
  from TString = return $ Type tstring
  from TUnit   = return $ Type tunit

instance Monad m => From m F.Type (Type i) where
  from (F.BaseType b)     = from b
  from (F.TArrow ty1 ty2) = do
    Type tq1 <- from ty1
    Type tq2 <- from ty2
    return $ Type $ tarrow tq1 tq2

data DynTerm repr h i = forall t. DynTerm (TQ i t) (repr h i t)

instance (Monad m, LSym repr) => From m Literal (DynTerm repr h i) where
  from (Int x)    = return $ DynTerm tint $ int x
  from (Bool x)   = return $ DynTerm tbool $ bool x
  from (String x) = return $ DynTerm tstring $ string x
  from Unit       = return $ DynTerm tunit $ unit

realize :: TSym (As t) => DynTerm repr h i -> Maybe (repr h i t)
realize (DynTerm (TQ ty) x) = as ty x

newtype V i t = V (TQ i t)

class Monad (m gamma h delta i) => EnvM m gamma h delta i where
  withBinding :: TQ i t -> m (V i t, gamma) (t, h) delta i a -> m gamma h delta i a
  lookupVar :: Symantics repr => F.Variable -> m gamma h delta i (Maybe (DynTerm repr h i))

newtype M gamma h delta i a = M { unM :: delta -> gamma -> Either String a }
  deriving (Functor)

instance Applicative (M gamma h delta i) where
  pure x = M $ const $ const $ pure x
  M f <*> M x = M $ \delta -> liftA2 (<*>) (f delta) (x delta)

instance Monad (M gamma h delta i) where
  M m >>= f = M $ \delta gamma -> either Left (\x -> unM (f x) delta gamma) $ m delta gamma

instance EnvM M () () delta i where
  withBinding tq (M m) = M $ \delta gamma -> m delta (V tq, gamma)
  lookupVar _ = return Nothing

instance EnvM M gamma h delta i => EnvM M (V i a, gamma) (a, h) delta i where
  withBinding tq (M m) = M $ \delta gamma -> m delta (V tq, gamma)
  lookupVar (F.Variable 0) = do
    (V tq, _) <- M $ const pure
    return $ Just $ DynTerm tq vz
  lookupVar (F.Variable n) = do
    x <- M $ \delta (_, gamma) -> unM (lookupVar (F.Variable $ n - 1)) delta gamma
    case x of
      Just (DynTerm tq t) -> return $ Just $ DynTerm tq $ vs t
      Nothing             -> return Nothing

throwError :: String -> M gamma h delta i a
throwError = M . const . const . Left

instance (EnvM M gamma h delta i, Symantics repr) => From (M gamma h delta i) Term (DynTerm repr h i) where
  from (Var v)    = lookupVar v >>= maybe (throwError $ "unbound variable: " ++ show v) return
  from (Abs ty t) = do
    Type ty1 <- from ty
    DynTerm tq x <- withBinding ty1 $ from t
    return $ DynTerm (tarrow ty1 tq) $ abs_ x
  from (App t1 t2) = do
    DynTerm ty1 x <- from t1
    DynTerm ty2 y <- from t2
    AsArrow _ m <- return $ getTQ ty1
    (ty11, ty12, eq) <- maybe (throwError "not function") return m
    y <- maybe (throwError "type mismatch") return $ cast ty2 y ty11
    return $ DynTerm ty12 $ app (getEquality eq x) y
  from (Lit l)         = from l
  from (Print t)       = from t >>= maybe (throwError "not string") (return . DynTerm tunit . pr) . realize
  from (Int2String t)  = from t >>= maybe (throwError "not integer") (return . DynTerm tstring . int2string) . realize
  from (Bool2String t) = from t >>= maybe (throwError "not boolean") (return . DynTerm tstring . bool2string) . realize
  from Add             = return $ DynTerm (tarrow tint $ tarrow tint tint) $ add
  from (Sub t1 t2)     = do
    x <- from t1 >>= maybe (throwError "not integer") return . realize
    y <- from t2 >>= maybe (throwError "not integer") return . realize
    return $ DynTerm tint $ sub x y
  from (If t1 t2 t3) = do
    x <- from t1 >>= maybe (throwError "not boolean") return . realize
    DynTerm ty2 y <- from t2
    DynTerm ty3 z <- from t3
    z <- maybe (throwError "type mismatch") return $ cast ty3 z ty2
    return $ DynTerm ty2 $ if_ x y z
  from (Concat t1 t2) = do
    x <- from t1 >>= maybe (throwError "not string") return . realize
    y <- from t2 >>= maybe (throwError "not string") return . realize
    return $ DynTerm tstring $ concat_ x y

convert' :: Symantics repr => Term -> M () () delta i (DynTerm repr () i)
convert' = from

convert :: Symantics repr => i -> Term -> Either String (DynTerm repr () i)
convert i = ($ ()) . ($ i) . unM . convert'

newtype R h i a = R { unR :: h -> E a }

lit :: a -> R h i a
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
  add           = R $ const $ pure $ \x -> pure $ \y -> pure $ x + y
  sub r1 r2     = R $ \env -> liftA2 (-) (unR r1 env) $ unR r2 env
  if_ r1 r2 r3  = R $ \env -> do
    b <- unR r1 env
    if b
      then unR r2 env
      else unR r3 env
  concat_ r1 r2 = R $ \env -> liftA2 (<>) (unR r1 env) $ unR r2 env

-- Evaluate a facade term in empty environment.
evalF :: i -> Term -> Either String (E ())
evalF i t = f <$> convert i t
  where
    f :: DynTerm R () i -> E ()
    f (DynTerm _ r) = void $ unR r ()
