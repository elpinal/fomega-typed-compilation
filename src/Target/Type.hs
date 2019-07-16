{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Target.Type
  ( BSym(..)
  , TSym(..)
  , TQ(..)
  , Equality(..)
  , As
  , as
  , AsArrow(..)
  , cast

  , E(..)
  ) where

import Control.Monad.IO.Class
import Data.Coerce

class BSym repr where
  tint :: repr i Int
  tbool :: repr i Bool
  tstring :: repr i String
  tunit :: repr i ()

newtype E a = E { unE :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

class BSym repr => TSym repr where
  tarrow :: repr i a -> repr i b -> repr i (a -> E b)
  impure :: repr i a -> repr i (E a)

-- A type quantifying over TSym interpreters.
newtype TQ i t = TQ { getTQ :: forall repr. TSym repr => repr i t }

instance BSym TQ where
  tint = TQ tint
  tbool = TQ tbool
  tstring = TQ tstring
  tunit = TQ tunit

instance TSym TQ where
  tarrow (TQ x) (TQ y) = TQ $ tarrow x y
  impure (TQ x) = TQ $ impure x

newtype Equality a b = Equality { getEquality :: forall c. c a -> c b }

refl :: Equality a a
refl = Equality id

trans :: Equality a u -> Equality u b -> Equality a b
trans eq1 eq2 = getEquality eq2 eq1

newtype Symm a b = Symm (Equality b a)

symm :: Equality a b -> Equality b a
symm eq = coerce $ getEquality eq $ Symm refl

newtype Y t a b = Y { unY :: Equality t (a -> b) }
newtype Z t a b = Z { unZ :: Equality t (b -> a) }

eqArrow :: Equality a1 a2 -> Equality b1 b2 -> Equality (a1 -> b1) (a2 -> b2)
eqArrow eq1 eq2 = unZ $ getEquality eq1 $ Z $ unY $ getEquality eq2 $ Y refl

newtype EqE t a = EqE { unEqE :: Equality t (E a) }

eqE :: Equality a b -> Equality (E a) (E b)
eqE eq = unEqE $ getEquality eq $ EqE refl

newtype As t i a = As (Maybe (Equality a t))

instance BSym (As Int) where
  tint    = As $ return refl
  tbool   = As Nothing
  tstring = As Nothing
  tunit   = As Nothing

instance TSym (As Int) where
  tarrow _ _ = As Nothing
  impure _   = As Nothing

instance BSym (As Bool) where
  tint    = As Nothing
  tbool   = As $ return refl
  tstring = As Nothing
  tunit   = As Nothing

instance TSym (As Bool) where
  tarrow _ _ = As Nothing
  impure _   = As Nothing

instance BSym (As String) where
  tint    = As Nothing
  tbool   = As Nothing
  tstring = As $ return refl
  tunit   = As Nothing

instance TSym (As String) where
  tarrow _ _ = As Nothing
  impure _   = As Nothing

instance BSym (As ()) where
  tint    = As Nothing
  tbool   = As Nothing
  tstring = As Nothing
  tunit   = As $ return refl

instance TSym (As ()) where
  tarrow _ _ = As Nothing
  impure _   = As Nothing

data AsArrow i a = forall b1 b2. AsArrow (TQ i a) (Maybe (TQ i b1, TQ i b2, Equality a (b1 -> E b2)))

instance BSym AsArrow where
  tint    = AsArrow tint Nothing
  tbool   = AsArrow tbool Nothing
  tstring = AsArrow tstring Nothing
  tunit   = AsArrow tunit Nothing

instance TSym AsArrow where
  tarrow (AsArrow ty1 _) (AsArrow ty2 _) = AsArrow (tarrow ty1 ty2) $ return (ty1, ty2, refl)
  impure (AsArrow ty _)                  = AsArrow (impure ty) Nothing

data AsImpure i a = forall b. AsImpure (TQ i a) (Maybe (TQ i b, Equality a (E b)))

instance BSym AsImpure where
  tint    = AsImpure tint Nothing
  tbool   = AsImpure tbool Nothing
  tstring = AsImpure tstring Nothing
  tunit   = AsImpure tunit Nothing

instance TSym AsImpure where
  tarrow (AsImpure ty1 _) (AsImpure ty2 _) = AsImpure (tarrow ty1 ty2) Nothing
  impure (AsImpure ty _)                   = AsImpure (impure ty) $ return (ty, refl)

as :: As t i a -> c a -> Maybe (c t)
as (As meq) x = ($ x) . getEquality <$> meq

asSymm :: TSym (As t) => TQ i b -> Maybe (Equality t b)
asSymm (TQ (As meq)) = symm <$> meq

newtype Cast i a = Cast (forall b. TQ i b -> Maybe (Equality a b))

instance BSym Cast where
  tint    = Cast asSymm
  tbool   = Cast asSymm
  tstring = Cast asSymm
  tunit   = Cast asSymm

instance TSym Cast where
  tarrow (Cast t1) (Cast t2) = Cast $ f . getTQ
    where
      f (AsArrow _ m) = do
        (ty1, ty2, eq) <- m
        x <- t1 ty1
        y <- t2 ty2
        return $ trans (eqArrow x $ eqE y) $ symm eq
  impure (Cast t) = Cast $ f . getTQ
    where
      f (AsImpure _ m) = do
        (ty, eq) <- m
        x <- t ty
        return $ trans (eqE x) $ symm eq

cast :: TQ i a -> c a -> TQ i b -> Maybe (c b)
cast (TQ (Cast f)) ca tb = ($ ca) . getEquality <$> f tb
