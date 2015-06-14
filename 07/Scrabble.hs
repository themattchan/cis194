{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char (toLower)
import Data.Monoid

newtype Score = Score Int deriving (Eq, Ord, Show, Num)

getScore (Score s) = s

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

charWeights = [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

score :: Char -> Score
score c = let scoreTable = zip ['a'..'z'] charWeights in
  case lookup (toLower c) scoreTable of
    Just s  -> Score s
    Nothing -> Score 0

scoreString :: String -> Score
scoreString = mconcat . map score

--scoreString = foldl mappend mempty . map score
