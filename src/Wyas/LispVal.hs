module Wyas.LispVal where

import Numeric
import Text.PrettyPrint.Leijen

data LispVal
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | List [LispVal]
  deriving (Eq, Show)

instance Pretty LispVal where
  pretty (Atom str) = pretty str
  pretty (Number i) = pretty i
  pretty (String s) = dquotes (pretty s)
  pretty (Bool True) = text "#t"
  pretty (Bool False) = text "#f"
  pretty (Character ' ') = text "#\\space"
  pretty (Character '\n') = text "#\\newline"
  pretty (Character c) = text ("#\\" ++ [c])
  pretty (Float d) = text (showFFloat Nothing d "")
  pretty (List ls) = enclose lparen rparen (pretty ls)
