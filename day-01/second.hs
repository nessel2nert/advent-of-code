import Data.Char (digitToInt, isDigit)
import Data.List (partition)
import System.IO (isEOF)

getLines :: IO [String]
getLines = do
  eof <- isEOF
  if eof
    then return []
    else do
      l <- getLine
      (l :) <$> getLines

main = do
  ls <- getLines
  print $ solve ls

solve :: [String] -> Int
solve = sum . map decode

decode s = 10 * firstNum nums nums s + firstNum revNums revNums (reverse s)

firstNum base index (x : xs) =
    if isDigit x
    then digitToInt x
    else
        let index' = base ++ (fstMap tail <$> filter ((== x) . head . fst) index)
            (matchList, index'') = partition (null . fst) index'
        in case matchList of
            [] -> firstNum base index'' xs
            (_, match) : _ -> match

fstMap f (x, y) = (f x, y)

nums =
  [ ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9)
  ]

revNums = fstMap reverse <$> nums
