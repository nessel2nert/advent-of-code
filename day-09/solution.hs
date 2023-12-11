main = do
    ls <- lines <$> readFile "input.txt"
    let first  = show $ sum $ map (findNext . parseLine) ls
        second = show $ sum $ map (findPrev . parseLine) ls  
    putStrLn $ "Part 1: " <> first
    putStrLn $ "Part 2: " <> second

parseLine :: String -> [Integer]
parseLine = map read . words

diff :: [Integer] -> [Integer]
diff []  = error "Empty list"
diff [_] = error "Singleton list"
diff xs  = zipWith (-) (tail xs) xs

findNext :: [Integer] -> Integer
findNext xs
    | all (==0) xs = 0
    | otherwise    = last xs + findNext (diff xs)

findPrev :: [Integer] -> Integer
findPrev xs
    | all (==0) xs = 0
    | otherwise    = head xs - findPrev (diff xs)