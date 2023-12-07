import Parser
import Control.Applicative (Alternative(many, some))
import Data.Maybe (isJust)

main = do
  ls <- getLines
  print $ sum $ ids $ map parse ls

ids = map gameId . filter belowLimit where
  belowLimit game = and $ do
    (count, color) <- rounds game
    (refColor, limit) <- limits
    return $ (color /= refColor) || (count <= limit)

colors = ["red", "green", "blue"]
limits = zip colors [12 :: Int, 13, 14] 

data Game = Game {
  gameId :: Int,
  rounds :: [(Int, String)]
} deriving Show

parse input = Game getId getRounds where
  Just (getId, rest) = runP (stringP "Game " *> intP <* stringP ": ") input
  getRounds = getP (many round) rest
  round = (,) <$> (intP <* spacesP) <*> (anyWord colors <* spacesP <* anyOf ";," <* spacesP) 
