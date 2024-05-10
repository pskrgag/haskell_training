module JSON.Parser (parse) where

import Control.Applicative
import Data.Char
import Debug.Trace
import Text.Read (readMaybe)

data Value
  = String String
  | Number Double
  | Object [(Value, Value)] -- an association list -- only a `String` is valid as the index `Value`
  | Array [Value] -- not limited to identical primitive datatypes
  | Boolean Bool -- either `True` or `False`
  | Null
  deriving (Show)

data Error = NotExptected
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> Either Error (a, String)}

instance Functor Parser where
  fmap fn (Parser f) =
    Parser
      ( \x ->
          do
            (v, other) <- f x
            return (fn v, other)
      )

instance Applicative Parser where
  -- pure v = Parser (Right . (v,))
  pure a = Parser (\str -> Right (a, str))

  (Parser fn) <*> (Parser f) =
    Parser
      ( \x -> do
          (fn', other) <- fn x
          (v, other') <- f other
          return (fn' v, other')
      )

instance Monad Parser where
  (>>=) (Parser f) fn =
    Parser
      ( \x -> do
          (v, other) <- f x
          runParser (fn v) other
      )

instance Alternative Parser where
  empty = Parser ((const . Left) NotExptected)
  (Parser fn) <|> (Parser fn') =
    Parser
      ( \x ->
          case fn x of
            Left _ -> fn' x
            Right e -> Right e
      )

parseCond :: (Char -> Bool) -> Parser Char
parseCond fn = Parser helper
  where
    helper [] = Left NotExptected
    helper (x : xs)
      | fn x = Right (x, xs)
      | otherwise = Left NotExptected

parseChar :: Char -> Parser Char
parseChar x = parseCond (== x)

parseString :: String -> Parser String
parseString = traverse parseChar

parseWhile :: Parser a -> Parser [a]
parseWhile p = liftA2 (:) p $ helper p
  where
    helper :: Parser a -> Parser [a]
    helper p = parseWhile p <|> pure []

parseSep :: Parser a -> Parser String -> Parser [a]
parseSep val sep = liftA2 (:) val (helper1 val sep)
  where
    helper2 val sep = liftA2 (:) (sep *> val) (helper1 val sep)
    helper1 val sep = helper2 val sep <|> pure []

skipSpace :: Parser String
skipSpace = parseWhile (parseChar ' ') <|> pure ""

-- Json objects

parseNull :: Parser Value
parseNull = Null <$ parseString "null"

-- intParser, doubleParser, negativeInt :: Parser String
--
-- negativeInt = fmap (++) (parseString "-") <*> (doubleParser <|> intParser)
-- intParser = parseWhile (parseCond isNumber)
-- doubleParser = fmap test intParser <*> parseString "." <*> intParser
-- test a b c = a ++ b ++ c

test :: Parser String
test = Parser helper
  where
    helper [x] = Right ("", [x])
    helper l@('0' : '.' : xs) = Right ("", l)
    helper l@('0' : x : xs) = if (not . isNumber) x then Right ("", l) else Left NotExptected
    helper xs = Right ("", xs)

parseInt :: Parser Value
parseInt =
  Number . read <$> (skipSpace *> (doubleParser <|> intParserNotFromZero <|> negativeInt))
  where
    intParser, doubleParser, negativeInt :: Parser String

    negativeInt = fmap (++) (parseString "-") <*> (doubleParser <|> intParser)
    intParserNotFromZero = test *> parseWhile (parseCond isNumber)
    intParser = parseWhile (parseCond isNumber)
    doubleParser = fmap concat intParser <*> parseString "." <*> intParser
    concat a b c = a ++ b ++ c

parseStringValue :: Parser Value
parseStringValue = String <$> (parseChar '\"' *> (parseWhile (parseCond (/= '\"')) <|> pure "") <* parseChar '\"')

parseBool :: Parser Value
parseBool = (\x -> if x == "true" then Boolean True else Boolean False) <$> (parseString "true" <|> parseString "false")

parseArray :: Parser Value
parseArray =
  Array
    <$> ( parseChar '['
            *> ((parseSep (skipSpace *> parseJson <* skipSpace) (parseString ",")) <|> pure [])
            <* parseChar ']'
        )

parsePair :: Parser (Value, Value)
parsePair =
  Parser
    ( \x -> do
        (str, other) <- runParser parseStringValue x
        (val, other') <- runParser (skipSpace *> parseChar ':' *> skipSpace *> parseJson) other
        return ((str, val), other')
    )

parseObject :: Parser Value
parseObject =
  Object
    <$> ( parseChar '{'
            *> ((parseSep (skipSpace *> parsePair <* skipSpace) (parseString ",")) <|> pure [])
            <* parseChar '}'
        )

parseJson :: Parser Value
parseJson = skipSpace *> (parseNull <|> parseBool <|> parseInt <|> parseStringValue <|> parseArray <|> parseObject) <* skipSpace

parse :: String -> Maybe Value
parse xs = (trace (show xs ++ "\n")) $
  case runParser parseJson xs of
    Left e -> Nothing
    Right (v, "") -> Just v
    Right (v, other) -> case parse other of
      Just v1 -> Just v
      Nothing -> Nothing
