import Control.Monad (guard)

newtype Galaxy = Galaxy (Integer, Integer) deriving Eq

emptyCols = foldr mark (repeat False) where
  mark l m = zipWith (||) m $ map (=='#') l
  
galaxize :: [String] -> [Galaxy]
galaxize ls = do
  let rows = ls >>= \l -> if all (=='.') l then [l,l] else [l]
      colMask = scanl (\x empty -> if not empty then x + 2 else x + 1) 0 $ emptyCols ls
  (y, outer) <- zip [0..] rows
  (x, inner) <- zip colMask outer
  guard $ inner == '#'
  return $ Galaxy (x,y)
  
distances :: [Galaxy] -> Integer
distances gs = sum $ do
  g0 <- gs
  g1 <- gs
  guard $ g0 /= g1
  return $ dist g0 g1 where
    dist (Galaxy (x0, y0)) (Galaxy (x1, y1)) = abs (x1-x0) + abs (y1-y0)
 
mask ls = scanl (\x empty -> if not empty then x + 1 else x + 2) 0 $ emptyCols ls

main = do
  ls <- lines <$> readFile "input.txt"
  print $ distances $ galaxize ls
