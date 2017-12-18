-- | A range that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.Range
  ( Position
  , positionRow
  , positionColumn
  , isBefore
  , mkPosition
  , Range
  , rangeStart
  , rangeEnd
  , mkRange
  , lineIntersectsWithRange
  ) where

data Position = Position
  { positionRow :: Int
  , positionColumn :: Int
  } deriving (Eq, Show)

isBefore :: Position -> Position -> Bool
p1 `isBefore` p2 =
  positionRow p1 < positionRow p2 || positionColumn p1 < positionColumn p2

mkPosition :: Int -> Int -> Maybe Position
mkPosition r c
  | r > 0 && c > 0 = Just (Position r c)
  | otherwise = Nothing

data Range = Range
  { rangeStart :: Position
  , rangeEnd :: Position
  } deriving (Eq, Show)

mkRange :: Position -> Position -> Maybe Range
mkRange s e
  | s `isBefore` e = Just (Range s e)
  | otherwise = Nothing

lineIntersectsWithRange :: Int -> Range -> Bool
lineIntersectsWithRange l (Range start end) =
  positionRow start <= l && positionRow end >= l
