{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck hiding ((===))

import Sized
import Scrabble
import Buffer
import Editor

-- Quickcheck interface
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

-- data Product = Product Int deriving (Functor)

instance Monoid Int where
  mempty  = 1
  mappend = (*)

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)


-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty y = y
(+++) x Empty = x
(+++) x y     = Append (mappend (tag x) (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single t _)   = t
tag (Append t _ _) = t


-- Exercise 2
-- TODO : write some QC tests

len :: (Sized b, Monoid b) => JoinList b a -> Int
len Empty = getSize mempty
len j     = getSize . size $ tag j

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ e)   = Just e
indexJ i (Append _ x y)
  | i < len x           = indexJ i x
  | otherwise           = indexJ (i - len x) y
indexJ _ _              = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i l | i <= 0 = l
dropJ i (Append _ x y)
  | i < len x = dropJ i x +++ y
  | otherwise = dropJ (i - len x) y
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i l | i <= 0 = Empty
takeJ i (Append _ x y)
  | i < len x = takeJ i x
  | otherwise = x +++ takeJ (i - len x) y
takeJ _ l = l



-- Exercise 3
-- also see Scrabble.hs

scoreLine :: String -> JoinList Score String
scoreLine l = Single (scoreString l) l

-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
   -- | Convert a buffer to a String.
  -- toString :: b -> String
  toString Empty          = ""
  toString (Single _ s)   = s
  toString (Append _ l r) = toString l ++ toString r

  -- | Create a buffer from a String.
  -- fromString :: String -> b
  fromString s = Single (scoreString s, Size 1) s

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  -- line :: Int -> b -> Maybe String
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  -- replaceLine :: Int -> String -> b -> b
  replaceLine n s b = takeJ (n-1) b +++ fromString s +++ dropJ n b

  -- | Compute the number of lines in the buffer.
  -- numLines :: b -> Int
  numLines = len

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  -- value :: b -> Int
  value Empty = 0
  value j     = getScore . fst $ tag j


main :: IO ()
main = runEditor editor buffer
  where
    -- Reify
    buffer :: JoinList (Score, Size) String
    buffer = Empty
