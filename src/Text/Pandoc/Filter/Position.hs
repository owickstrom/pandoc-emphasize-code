{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A position that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.Position
  ( Line(..)
  , Column(..)
  , Position
  , line
  , column
  , mkPosition
  , positionToTuple
  ) where

import Data.Hashable

newtype Line =
  Line Word
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Hashable)

newtype Column =
  Column Word
  deriving (Show, Eq, Ord, Num, Enum, Real, Integral, Hashable)

data Position = Position
  { line   :: Line
  , column :: Column
  } deriving (Eq, Show)

instance Ord Position where
  Position r1 c1 `compare` Position r2 c2
    | r1 < r2 = LT
    | r1 == r2 && c1 == c2 = EQ
    | r1 == r2 && c1 < c2 = LT
    | otherwise = GT

mkPosition :: Line -> Column -> Maybe Position
mkPosition r c
  | r > 0 && c > 0 = Just (Position r c)
  | otherwise = Nothing

positionToTuple :: Position -> (Line, Column)
positionToTuple (Position l c) = (l, c)
