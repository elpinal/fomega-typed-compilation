module Facade.Term
  ( Variable(..)
  , Literal(..)
  , Term(..)

  , module Facade.Type
  ) where

import Facade.Type

newtype Variable = Variable Int
  deriving (Eq, Ord, Show)

data Literal
  = Int Int
  | Bool Bool
  | String String
  deriving (Eq, Show)

data Term
  = Var Variable
  | Abs Type Term
  | App Term Term
  | Lit Literal
  | Print Term
  | Int2String Term
  | Bool2String Term
  | Sub Term Term
  | If Term Term Term
  | Concat Term Term
  deriving (Eq, Show)
