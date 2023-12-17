{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parser

data Hotspring =
    W | B | U
    deriving Show

data Arrangement = Arr [Hotspring] [Int]
    deriving Show

parse = run $ do
    let toHotspring = \case
            '.' -> W
            '#' -> B
            '?' -> U
    hs <- many $ toHotspring <$> anyChar ".#?"
    spaces
    ints <- (:) <$> int <*> many (char ',' *> int)  
    return $ Arr hs ints

main = do
    ls <- lines <$> readFile "input.txt"
    traverse print $ take 10 $ map parse ls