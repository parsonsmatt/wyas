module Wyas.LispVal where

import Numeric
import Text.PrettyPrint.Leijen
import Text.Show.Functions

instance Eq ((->) a b) where
  _ == _ = False

data LispVal
  = Atom String
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | List [LispVal]
  | Fn ([LispVal] -> LispVal)
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
  -- | This needs to print things with parens and no commas.
  -- >>> pretty (List [Atom "asdf", Number 3])
  -- ( "asdf" 3 )
  pretty (List ls) = encloseSep lparen rparen space (map pretty ls)
  pretty (Fn _) = text "<function>"

truthy :: LispVal -> Bool
truthy (Bool False) = False
truthy _ = True
