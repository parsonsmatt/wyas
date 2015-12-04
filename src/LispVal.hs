module LispVal where

import Numeric

data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    deriving (Eq)

showVal :: LispVal -> String
showVal (String s) = '"' : s ++ "\""
showVal (Atom n) = n
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List c) = "(" ++ unwordsList c ++ ")"
showVal (DottedList x xs) = "(" ++ unwordsList x ++ " . " ++ showVal xs ++ ")"
showVal (Character c) = "#\\" ++ [c]
showVal (Float d) = showFFloat Nothing d ""

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal


instance Show LispVal where
  show = showVal

