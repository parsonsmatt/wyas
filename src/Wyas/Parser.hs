module Wyas.Parser where

import Data.Char
import Text.ParserCombinators.Parsec hiding (string)
import Data.List (foldl')
import Numeric

import Wyas.LispVal

-- $setup
-- >>> import Data.Either (isLeft)

parseFile :: FilePath -> IO (Either ParseError LispVal)
parseFile f = do
    str <- readFile f
    case runParser lispExpr () f str of
         Left err -> return (Left err)
         Right v -> return (Right v)

parseLisp :: Parser LispVal -> String -> Either ParseError LispVal
parseLisp p = runParser p () ""

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- | Parses a String, consisting of many characters in quotes.
--
-- >>> parseLisp string "\"Hello world!\""
-- Right (String "Hello world!")
--
-- >>> parseLisp string " No quotes?? "
-- Left (line 1, column 1):
-- unexpected " "
-- expecting "\""
string :: Parser LispVal
string = String <$> (char '"' *> many safeQuotes <* char '"')

-- | Parses a character literal.
--
-- >>> parseLisp character "#\\space"
-- Right (Character ' ')
--
-- >>> parseLisp character "#\\a"
-- Right (Character 'a')
character :: Parser LispVal
character = char '#' >> char '\\' >> Character <$> lispChar
  where
    lispChar = spaceStr
        <|> newLineStr
        <|> anyChar
    spaceStr = stringCaseInsensitive "space" >> return ' '
    newLineStr = stringCaseInsensitive "newline" >> return '\n'


-- | Parses a list of lisp values separated by spaces.
--
-- >>> parseLisp list "1 2.0 (asdf \"hello\")"
-- Right (List [Number 1,Float 2.0,List [Atom "asdf",String "hello"]])
--
-- >>> parseLisp list "(print (\"hello\" \"world\"))"
-- Right (List [List [Atom "print",List [String "hello",String "world"]]])
list :: Parser LispVal
list = List <$> sepBy lispExpr spaces


quoted :: Parser LispVal
quoted = do
  x <- char '\'' *> lispExpr
  return $ List [Atom "quote", x]

float :: Parser LispVal
float = do
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

-- | Parses an Atom. This represents a symbol in Lisp which can be either
-- a keyword or variable.
--
-- >>> parseLisp atom "hello"
-- Right (Atom "hello")
--
-- >>> parseLisp atom "+"
-- Right (Atom "+")
--
-- >>> isLeft (parseLisp atom "#\a")
-- True
--
-- >>> parseLisp atom "#t"
-- Right (Bool True)
atom :: Parser LispVal
atom = do
  first <- letter <|> symbol
  case first of
       '#' -> (Bool . (=='t')) <$> oneOf "tf"
       _ -> do
         rest <- many (letter <|> digit <|> symbol)
         let atom' = first : rest
         case atom' of
              "#t" -> return $ Bool True
              "#f" -> return $ Bool False
              '#' : '\\' : _  -> fail "wrong"
              _ -> return $ Atom atom'

int :: Parser LispVal
int = try altBase <|> do
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

lispExpr :: Parser LispVal
lispExpr = pzero
  <|> string
  <|> try float
  <|> try int
  <|> try atom
  <|> character
  <|> quoted
  <|> char '(' *> list <* char ')'
