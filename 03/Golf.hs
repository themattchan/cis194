{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map (map snd) $ zipWith filter filters (replicate len ids)
  where len     = length xs
        ids     = zip [1..len] xs
        filters = map (\n -> (== 0) . flip mod n . fst) [1..len]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = map (!! 1) . filter maxs . take ((length xs) - 2) $ subs xs
  where subs xs        = xs : subs (tail xs)
        maxs (x:y:z:_) = y > x && y > z

histogram :: [Integer] -> String
histogram = (++ fmt) . concatMap barRow . reverse . rows . toAList
  where fmt = "==========\n0123456789\n"

type AList = [(Integer, Int)]

toAList :: [Integer] -> AList
toAList = map (\x -> (head x, length x)) . group . sort

rows :: AList -> [AList]
rows xs = if all ((<= 0) . snd) xs then []
          else xs : rows (map (fmap pred) xs)

barRow :: AList -> String
barRow xs = (++"\n") $ concatMap f [0..9]
  where f x = case lookup x xs of
                Just n | n > 0 -> "*"
                _              -> " "
