{-# LANGUAGE LambdaCase #-}

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
    spaced,
) where

import Data.Char
import Control.Applicative
import Data.Maybe (fromMaybe)
import Control.Monad (guard, void)
import Data.List

newtype Parser a = Parser { run :: String -> Maybe (a, String) }

-- Returns the parsed result throwing an exception if the parser failed
unwrap p = fst . fromMaybe (error "Unwrapped failed parser") . run p

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

-- Transform the given parser that parses a fallback value if it fails
parseOrElse :: a -> Parser a -> Parser a
parseOrElse fallback p = p <|> pure fallback


-- -- Parse a signed Integer given in octal, binary, hexadecimal and decimal
int :: Parser Int
int = do
    sign <- parseOrElse id $ negate <$ char '-' <|> id <$ char '+'
    base <- parseOrElse 10 (toBase <$> anyWord ["0x", "0b", "0o", "0d"])
    let digitList = take base $ zip "0123456789ABCDEF" [0..]
        fromValidDigit base c = snd $ head $ dropWhile ((/=c) . fst) $ take base digitList
    digits <- map (fromValidDigit base) <$> nonNull (many $ anyChar (map fst digitList))
    return $ sign $ foldl (\acc x -> base * acc + x) 0 digits
    where
    toBase :: String -> Int 
    toBase = \case
        "0x" -> 16
        "0b" -> 2
        "0o" -> 8
        "0d" -> 10

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
