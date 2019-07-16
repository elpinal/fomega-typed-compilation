module Facade.Type
  ( BaseType(..)
  , Type(..)
  ) where

data BaseType
  = TInt
  | TBool
  | TString
  | TUnit
  deriving (Eq, Show)

newtype TVariable = TVariable Int
  deriving (Eq, Show)

data Type
  = BaseType BaseType
  | TArrow Type Type
  | Forall Type
  | TVar TVariable
  deriving (Eq, Show)
