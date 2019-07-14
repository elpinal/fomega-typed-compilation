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

data Type
  = BaseType BaseType
  | TArrow Type Type
  deriving (Eq, Show)
