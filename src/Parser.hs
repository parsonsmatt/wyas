module Parser where

import LispVal
import Data.Char
import Text.ParserCombinators.Parsec
import Data.List (foldl')
import Numeric

-- $setup
-- >>> let testP p = runParser p () ""

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Parses a String, consisting of many characters in quotes.
--
-- >>> testP parseString "\"Hello world!\""
-- Right (String "Hello world!")
--
-- >>> testP parseString " No quotes?? "
-- Left (line 1, column 1):
-- unexpected " "
-- expecting "\""
parseString :: Parser LispVal
parseString = String <$> (char '"' *> many safeQuotes <* char '"')

-- | Parses a character literal.
--
-- >>> testP parseCharacter "#\\space"
-- Right (Character ' ')
--
-- >>> testP parseCharacter "#\\a"
-- Right (Character 'a')
parseCharacter :: Parser LispVal
parseCharacter = char '#' >> char '\\' >> Character <$> lispChar
    where
        lispChar = spaceStr
               <|> newLineStr
               <|> anyChar
        spaceStr = stringCaseInsensitive "space" >> return ' '
        newLineStr = stringCaseInsensitive "newline" >> return '\n'


-- | Parses a list of lisp values separated by spaces.
--
-- >>> testP parseList "1 2.0 (asdf \"hello\")"
-- Right (List [Number 1,Float 2.0,List [Atom "asdf",String "hello"]])
--
-- >>> testP parseList "(print (\"hello\" \"world\"))"
-- Right (List [List [Atom "print",List [String "hello",String "world"]]])
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

-- | Parses a list in a dotted format.
--
-- >>> testP parseDottedList "car . cdr"
-- Right (DottedList [Atom "car"] (Atom "cdr"))
parseDottedList :: Parser LispVal
parseDottedList = do
    car <- endBy parseExpr spaces
    cdr <- char '.' >> spaces >> parseExpr
    return (DottedList car cdr)

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\\' *> parseExpr
    return $ List [Atom "quote", x]

parseFloat :: Parser LispVal
parseFloat = do
    c <- option '+' (oneOf "+-")
    digits <- many1 digit
    post <- char '.' >> many1 digit
    return (Float ((if c == '+' then id else negate) (read (concat [digits, ".", post]))))

charCaseInsensitive :: Char -> Parser Char
charCaseInsensitive c = char (toLower c) <|> char (toUpper c)
    
stringCaseInsensitive :: String -> Parser String
stringCaseInsensitive s = try (mapM charCaseInsensitive s) <?> "\"" ++ s ++ "\""


safeQuotes :: Parser Char
safeQuotes = nonQuoteCharacter <|> specialCharacter

nonQuoteCharacter :: Parser Char
nonQuoteCharacter = do
    c <- noneOf "\"\\"
    case c of
         '"' -> fail "quotes"
         '\\' -> fail "backslash"
         _ -> return c

specialCharacter :: Parser Char
specialCharacter = do
    _ <- char '\\'
    c <- oneOf "nrt\\\""
    return $ case c of
                  'n' -> '\n'
                  'r' -> '\r'
                  't' -> '\t'
                  '\\' -> '\\'
                  '"' -> '\"'
                  _ -> error "unexpect"

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first : rest
    return $ case atom of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  _    -> Atom atom

parseInt :: Parser LispVal
parseInt = try altBase <|> do
  s <- try (char '-') <|> return '0'
  d <- many1 digit
  return (Number (read (s:d)))
  
altBase :: Parser LispVal
altBase = do
    c <- char '#' >> oneOf "bodx"
    n <- case c of
              'b' -> many (oneOf "01")
              'o' -> many (oneOf "01234567")
              'd' -> many digit
              'x' -> many (digit <|> oneOf "abcdefABCDEF")
              _   -> fail "wrong prefix"
    let number = case c of
                      'b' -> readBinary n
                      'o' -> fst . head . readOct $ n
                      'd' -> read n
                      'x' -> fst . head . readHex $ n
                      _   -> error "Wrong prefix"
    return (Number (fromIntegral number))

readBinary :: String -> Int
readBinary = foldl' (\acc c -> (acc * 2) + digitToInt c) 0

parseExpr :: Parser LispVal
parseExpr = pzero
    <|> parseCharacter
    <|> parseAtom
    <|> parseString
    <|> try parseFloat
    <|> parseInt
    <|> parseQuoted
    <|> do _ <- char '('
           x <- try parseList <|> parseDottedList
           _ <- char ')'
           return x
