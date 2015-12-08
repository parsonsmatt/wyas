module Wyas.Expr where

data Literal
  = Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  deriving (Eq, Show)

data Expr
  = Lit Literal
  | Atom String
  | List [Expr]
  deriving (Eq, Show)
