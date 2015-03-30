module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map (map snd) $ zipWith filter filters (replicate len ids)
  where len     = length xs
        ids     = zip [1..len] xs
        filters = map (\n (a,_) -> a `mod` n == 0) [1..len]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (!!1) . filter maxs . take ((length xs) - 2) $ subs xs
  where subs xs        = xs : subs (tail xs)
        maxs (x:y:z:_) = y > x && y > z

histogram :: [Integer] -> String
histogram = (++ fmt) . concatMap toStr . reverse . rows . assoc
  where fmt = "==========\n0123456789\n"

type Assoc a = [(a, Int)]

assoc :: (Ord a) => [a] -> Assoc a
assoc = map (\x -> (head x, length x)) . group . sort

rows :: Assoc a -> [Assoc a]
rows xs = if all ((==0) . snd) xs then []
          else xs : rows (map (\(n,c) -> (n, if c > 0 then c-1 else c)) xs)

toStr :: Assoc Integer -> String
toStr xs = (++"\n") $ concatMap f [0..9]
  where f x = case lookup x xs of
                Just 0  -> " "
                Just _  -> "*"
                Nothing -> " "
