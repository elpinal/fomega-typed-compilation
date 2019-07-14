{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Target.Type
  ( BSym(..)
  , TSym(..)
  , TQ(..)
  , asString
  ) where

class BSym repr where
  tint :: repr Int
  tbool :: repr Bool
  tstring :: repr String
  tunit :: repr ()

class BSym repr => TSym repr where
  tarrow :: repr a -> repr b -> repr (a -> b)

-- A type quantifying over TSym interpreters.
newtype TQ t = TQ { getTQ :: forall repr. TSym repr => repr t }

instance BSym TQ where
  tint = TQ tint
  tbool = TQ tbool
  tstring = TQ tstring
  tunit = TQ tunit

instance TSym TQ where
  tarrow (TQ x) (TQ y) = TQ $ tarrow x y

newtype Equality a b = Equality { getEquality :: forall c. c a -> c b }

refl :: Equality a a
refl = Equality id

newtype As t a = As (Maybe (Equality a t))

instance BSym (As Int) where
  tint    = As $ return refl
  tbool   = As Nothing
  tstring = As Nothing
  tunit   = As Nothing

instance TSym (As Int) where
  tarrow _ _ = As Nothing

instance BSym (As String) where
  tint    = As Nothing
  tbool   = As Nothing
  tstring = As $ return refl
  tunit   = As Nothing

instance TSym (As String) where
  tarrow _ _ = As Nothing

asString :: As String a -> c a -> Maybe (c String)
asString (As meq) x = ($ x) . getEquality <$> meq
