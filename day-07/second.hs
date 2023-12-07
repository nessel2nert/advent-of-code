import Parser
import Control.Applicative (Alternative(many))
import Data.List (sort, group, sortBy)
import Data.Ord (comparing, Down (Down))

main = do
  ls <- lines <$> readFile "input.txt"
  let
    sortedBets = map bet $ sort $ map parse ls
    winnings = sum $ zipWith (*) [1..] sortedBets
  print winnings

data Hand = Hand {cards :: [Card], bet :: Int}
    deriving (Eq)

newtype Card = Card {value :: Int}
    deriving (Eq, Ord)

instance Ord Hand where
    compare h1 h2 =
        case compare (rank h1) (rank h2) of
            EQ    -> compare (cards h1) (cards h2)
            notEq -> notEq

toCard c = Card $ case c of
    'A' -> 13
    'K' -> 12
    'Q' -> 11
    'T' -> 10
    '9' -> 9
    '8' -> 8
    '7' -> 7
    '6' -> 6
    '5' -> 5
    '4' -> 4
    '3' -> 3
    '2' -> 2
    'J' -> 1
    _   -> error "Couldn't match card"

rank hand =
    let pattern = sort . map length . group . sort . map value . cards
        jokerCount = head $ pattern hand
    in case tail (pattern hand) of
        []   -> 7
        [x]  -> 7
        x:xs -> rank' $ x + jokerCount : xs

rank' amounts = case amounts of 
    [5]       -> 7 -- Five of a kind
    [4,1]     -> 6 -- Four of a kind
    [3,2]     -> 5 -- Fullhouse
    [3,1,1]   -> 4 -- Three of a kind
    [2,2,1]   -> 3 -- Two pair
    [2,1,1,1] -> 2 -- Pair
    _         -> 1 -- High card


parse = unwrap $ Hand
    <$> many (toCard <$> anyChar "AKQJT98765432")
    <* spaces
    <*> int