module Parser (
    Parser(Parser),
    run,
    unwrap,
    char,
    string,
    spaces,
    parseWhile,
    int,
    anyChar,
    anyWord,
    spaced
) where

import Data.Char
import Control.Applicative
import Data.Maybe (fromJust)
import Control.Monad (guard, void)

newtype Parser a = Parser { run :: String -> Maybe (a, String) }

-- Returns the parsed result throwing an exception if the parser failed
unwrap p = fst . fromJust . run p

-- Parses a single char
char :: Char -> Parser Char
char c = Parser f where
    f [] = Nothing
    f (x:xs)
        | x == c = Just (x, xs)
        | otherwise = Nothing

-- Parses a single char
anyChar :: String -> Parser Char
anyChar = foldr ((<|>) . char) empty

-- Parse any of the given strings, if multiple strings match the input, the first one is taken.
anyWord :: [String] -> Parser String
anyWord = foldr ((<|>) . string) empty

-- Parse the given string
string :: String -> Parser String
string = traverse char

-- Parse prefix satisfying the predicate
parseWhile :: (Char -> Bool) -> Parser String
parseWhile cond = Parser $ Just . span cond

-- Takes a parsed list and asserts it's not null
nonNull :: Parser [a] -> Parser [a]
nonNull (Parser p) = Parser p' where
    p' input = do
        (result, rest) <- p input
        if null result
        then Nothing
        else return (result, rest)

-- Remove spaces from the head of the input
spaces :: Parser ()
spaces = void $ parseWhile isSpace

-- Transform the given parser into one ignoring surrounding spaces
spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces 

-- Give a Parse pattern for the separator and for the values to parse inbetween
sepBy :: Parser String -> Parser a -> Parser [a]
sepBy sep p = ((:) <$> p <*> many (sep *> p)) <|> pure []

-- Parse a signed Int
int :: Parser Int
int = signP <*> (read <$> nonNull (parseWhile isDigit)) where
    signP =
        negate <$ char '-'
        <|> id <$ char '+'
        <|> pure id

instance Functor Parser where
    fmap f (Parser p) = Parser p' where
        p' input = do
            (result, next) <- p input
            return (f result, next)

instance Applicative Parser where
    pure x = Parser $ Just . (x,)
    (Parser p1) <*> (Parser p2) = Parser p where
        p input = do
            (f, next) <- p1 input
            (result, next) <- p2 next
            return (f result, next)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser f where
        f input = p1 input <|> p2 input

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser p' where
        p' input = do
            (result, next) <- p input
            (result, next) <- run (f result) next
            return (result, next)
