import Parser
import Data.Char (digitToInt, isDigit)
import Control.Applicative

main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    print $ sum $ solve ls

-- fold3 over 
solve :: [String] -> [Int]
solve xs = solve' (dots:xs)
    where
        dots = '.' : dots
        solve' [x1, x2] = scan x1 x2 dots
        solve' (x1:x2:x3:xs) =
            scan x1 x2 x3
            <> solve' (x2:x3:xs) 

-- initialize scanTriples
scan :: String -> String -> String -> [Int]
scan xs ys zs = scanTriples (zip3 xs ys zs) False Nothing []

-- compare heads in triples taking any matching numbers from the middle item
scanTriples :: [(Char, Char, Char)] -> Bool -> Maybe Int -> [Int] -> [Int]
scanTriples [] sym currInt commits =
    case (currInt, sym) of
        (Just x, True)  -> x:commits
        _               -> commits  
scanTriples ((top, mid, bot):xs) sym currInt commits = 
    if isDigit mid
    then let
        midNum = digitToInt mid
        updatedInt = fmap (\x -> 10 * x + midNum) currInt <|> Just (digitToInt mid)
        in scanTriples xs (sym' || sym) updatedInt commits 
    else let
        updatedCommits = if sym' then case currInt of { Nothing -> commits; Just x -> x:commits } else commits
        in case (top, mid, bot) of
            ('.', '.', '.') -> scanTriples xs False Nothing updatedCommits
            _               -> scanTriples xs sym' Nothing updatedCommits
    where 
        sym' = sym || isSym top || isSym mid || isSym bot
        isSym x = not (isDigit x || x == '.')

