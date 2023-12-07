import Control.Applicative
import Data.Functor
import Data.List
import Parser

main = do
  ls <- lines <$> readFile "input.txt"
  print $ play $ toCardList $ map matches ls

matches :: String -> Int
matches = length . uncurry intersect . parse

toCardList :: [Int] -> [(Int, Int)]
toCardList = map (,1)

play :: [(Int, Int)] -> Int
play xs = case xs of
    [] -> 0
    (match, count) : xs ->
        let updateCount (match, count') = (match, count + count')
        in count + play (map updateCount (take match xs) <> drop match xs)

parse :: String -> ([Int], [Int])
parse =
  unwrap $
    (,)
      <$> ( parseWhile (/= ':')
              *> char ':'      -- ignore prefix
              *> ints         -- card numbers
              <* spaced (char '|')
          )
      <*> ints                -- winning numbers
  where
    ints = many (spaced int)
