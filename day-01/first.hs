import Data.Char (digitToInt, isDigit)
import Data.List
import System.IO (isEOF)

main = do
  ls <- lines <$> readFile "input.txt"
  print $ solve ls

solve :: [String] -> Int
solve = sum . map decode
  where
    decode s = 10 * firstDigit s + firstDigit (reverse s)
    firstDigit = digitToInt . head . dropWhile (not . isDigit)