import Control.Monad (guard)

newtype Galaxy = Galaxy (Integer, Integer) deriving (Show, Eq)

emptyCols = foldr mark (repeat False) where
  mark l m = zipWith (||) m $ map (=='#') l
  
galaxize :: Integer -> [String] -> [Galaxy]
galaxize expansion ls = do
  let rowMask = scanr (\line y -> if all (=='.') line then y + expansion else y + 1) 0 ls 
      colMask = scanl (\x hasGalaxy -> if not hasGalaxy then x + expansion else x + 1) 0 $ emptyCols ls
  (y, outer) <- zip rowMask ls
  (x, inner) <- zip colMask outer
  guard $ inner == '#'
  return $ Galaxy (x,y)

distances :: [Galaxy] -> Integer
distances gs = flip div 2 $ sum $ do
  g0 <- gs
  dist g0 <$> gs where
    dist (Galaxy (x0,y0)) (Galaxy (x1,y1)) = abs (x1-x0) + abs (y1-y0)

main = do
    ls <- lines <$> readFile "input.txt"
    let first  = show $ distances $ galaxize 2 ls
        second = show $ distances $ galaxize 1000000 ls  
    putStrLn $ "Part 1: " <> first
    putStrLn $ "Part 2: " <> second

