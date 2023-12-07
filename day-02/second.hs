import Parser
import Control.Applicative (Alternative(many))

main = do
  ls <- lines <$> readFile "input.txt"
  print $ sum . map (powers . rounds . parse) $ ls

powers :: [(Int, String)] -> Int
powers rs = product $ do
  c <- colors
  return $ maximum $ map fst $ filter ((==c) . snd) rs

colors = ["red", "green", "blue"]

data Game = Game {
  gameId :: Int,
  rounds :: [(Int, String)]
} deriving Show

parse input = Game getId getRounds where
  Just (getId, rest) = run (string "Game " *> int <* string ": ") input
  getRounds = unwrap (many round) rest
  round = (,) <$> (int <* spaces) <*> (anyWord colors <* spaces <* anyChar ";," <* spaces) 
