module Hw4 where

-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f x = if even x then x `div` 2 else 3*x+1


-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

insElem :: a -> Tree a -> Tree a
insElem e Leaf = Node 0 Leaf e Leaf
insElem e (Node h l e' r)
  | height l < height r =
      let l' = (insElem e l) in
      Node (height l') l' e' r
  | height r < height l =
      let r' = (insElem e r) in
      Node (height r') l e' r'
  where
    height Leaf           = -1
    height (Node h _ _ _) = h

foldTree :: [a] -> Tree a
foldTree = foldr insElem Leaf


-- Exercise 3
xor :: [Bool] -> Bool
xor = odd . length . foldr (\x a -> if x then x:a else a) []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f b xs = foldr (\x a b -> a (f b x)) id xs b


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram = undefined

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
