module Fibonacci where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream e s) = e : streamToList s

instance Show a => Show (Stream a) where
  show = (++",...]") . init . show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat e = Stream e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream e s) = Stream (f e) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s $ streamFromSeed f (f s)

-- Exercise 5
nats :: Stream Integer
nats = go 0
  where go n = Stream n $ go (succ n)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream e1 s1) (Stream e2 s2) =
  Stream e1 $ Stream e2 $ interleaveStreams s1 s2
