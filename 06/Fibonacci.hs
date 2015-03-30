{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-@ LIQUID "--no-termination" @-}
module Fibonacci where
import Data.List (unzip4)

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
  show = ("Stream "++) . (++",...") . init . tail . show . take 20 . streamToList

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat e = Stream e (streamRepeat e)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream e s) = Stream (f e) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream s $ streamFromSeed f (f s)

streamZipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZipWith f (Stream a as) (Stream b bs) =
  Stream (f a b) $ streamZipWith f as bs

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream e1 s1) s2 = Stream e1 $ interleaveStreams s2 s1

ruler :: Stream Integer
ruler = go 0
  where go n = interleaveStreams (streamRepeat n) (go $ succ n)

-- Exercise 6
x :: Stream Integer
x = Stream 0 . Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger = flip Stream (streamRepeat 0)
  negate      = streamMap negate
  (+)         = streamZipWith (+)

  (*) (Stream a as') bs@(Stream b bs') =
    Stream (a*b) $ (+) (streamMap (*a) bs') (as' * bs)

instance Fractional (Stream Integer) where
  (/) (Stream a as) (Stream b bs) = q
    where q = Stream (a `div` b) $ streamMap (`div` b) (as - q*bs)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
newtype Matrix = Matrix (Integer, Integer, Integer, Integer)
               deriving (Show, Eq)

toTup4  a                   = (a,a,a,a)
mapTup4 f         (a,b,c,d) = (f a, f b, f c, f d)
zipApp4 (f,g,h,i) (a,b,c,d) = (f a, g b, h c, i d)

{-
instance Show Matrix where
  show (Matrix (a,b,c,d)) =
    "[ " ++ show a ++ " " ++ show b ++ " ]\n" ++
    "[ " ++ show c ++ " " ++ show d ++ " ]"
-}

instance Num Matrix where
  fromInteger       = Matrix . toTup4
  negate (Matrix a) = Matrix $ mapTup4 negate a

  (+) (Matrix a) (Matrix b) =
    Matrix $ mapTup4 sum (unzip4 [a,b])

  (*) (Matrix (a,b,c,d)) (Matrix (e,f,g,h)) =
    Matrix (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = snd $ f1^n
  where f1 = Matrix (1,1,1,0)
        snd (Matrix (_,b,_,_)) = b
