import Parser
import Control.Applicative (Alternative(many))

main = do
  ls <- getLines
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
  Just (getId, rest) = runP (stringP "Game " *> intP <* stringP ": ") input
  getRounds = getP (many round) rest
  round = (,) <$> (intP <* spacesP) <*> (anyWord colors <* spacesP <* anyOf ";," <* spacesP) 
