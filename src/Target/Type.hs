module Target.Type
  ( BSym(..)
  , TSym(..)
  ) where

class BSym repr where
  tint :: repr Int
  tbool :: repr Bool
  tstring :: repr String
  tunit :: repr ()

class BSym repr => TSym repr where
  tarrow :: repr a -> repr b -> repr (a -> b)
