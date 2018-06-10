{-# LANGUAGE CPP #-}

-- | Ranges that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.EmphasizeCode.Range
  ( Range
  , rangeStart
  , rangeEnd
  , mkRange
  , rangeToTuple
  , rangeToTuples
  , lineIntersectsWithRange
  , Ranges
  , rangesToList
  , RangesError(..)
  , mkRanges
  , SingleLineRange
  , singleLineRangeLine
  , singleLineRangeStart
  , singleLineRangeEnd
  , mkSingleLineRange
  , splitRanges
  ) where
#if MIN_VERSION_base(4,8,0)
import           Data.Semigroup                            ((<>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Control.Monad                             (foldM_, when)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HashMap
import           Data.List                                 (sortOn)

import           Text.Pandoc.Filter.EmphasizeCode.Position

data Range = Range
  { rangeStart :: Position
  , rangeEnd   :: Position
  } deriving (Eq, Show)

mkRange :: Position -> Position -> Maybe Range
mkRange s e
  | s <= e = Just (Range s e)
  | otherwise = Nothing

rangeToTuple :: Range -> (Position, Position)
rangeToTuple (Range p1 p2) = (p1, p2)

rangeToTuples :: Range -> ((Line, Column), (Line, Column))
rangeToTuples r =
  let (p1, p2) = rangeToTuple r
  in (positionToTuple p1, positionToTuple p2)

rangesAreDisjoint :: Range -> Range -> Bool
rangesAreDisjoint (Range s1 e1) (Range s2 e2) =
  (s1 < s2 && e1 < e2) || (s2 < s1 && e2 < e1)

rangeIntersects :: Range -> Range -> Bool
rangeIntersects r1 r2 = not (rangesAreDisjoint r1 r2)

lineIntersectsWithRange :: Line -> Range -> Bool
lineIntersectsWithRange l (Range start end) = line start <= l && line end >= l

newtype Ranges =
  Ranges [Range]
  deriving (Eq, Show)

rangesToList :: Ranges -> [Range]
rangesToList (Ranges rs) = rs

data RangesError
  = EmptyRanges
  | Overlap Range
            Range
  deriving (Show, Eq)

mkRanges :: [Range] -> Either RangesError Ranges
mkRanges [] = Left EmptyRanges
mkRanges ranges = do
  let sorted = sortOn rangeStart ranges
  foldM_ checkOverlap Nothing sorted
  pure (Ranges sorted)
  where
    checkOverlap (Just last') this = do
      when (last' `rangeIntersects` this) $ Left (Overlap last' this)
      return (Just this)
    checkOverlap Nothing this = return (Just this)

data SingleLineRange = SingleLineRange
  { singleLineRangeLine  :: Line
  , singleLineRangeStart :: Column
  , singleLineRangeEnd   :: Maybe Column
  } deriving (Eq, Show)

mkSingleLineRange :: Line -> Column -> Maybe Column -> Maybe SingleLineRange
mkSingleLineRange line' start (Just end)
  | line' > 0 && start < end = Just (SingleLineRange line' start (Just end))
mkSingleLineRange line' start Nothing
  | line' > 0 = Just (SingleLineRange line' start Nothing)
mkSingleLineRange _ _ _ = Nothing

rangeToSingleLineRanges :: Range -> [SingleLineRange]
rangeToSingleLineRanges r@Range {rangeStart = p1, rangeEnd = p2}
  | line p1 == line p2 =
    [SingleLineRange (line p1) (column p1) (Just (column p2))]
  | line p2 > line p1 =
    let startLine = SingleLineRange (line p1) (column p1) Nothing
        endLine = SingleLineRange (line p2) 1 (Just (column p2))
        middleLines =
          [ SingleLineRange n 1 Nothing
          | n <- [succ (line p1) .. pred (line p2)]
          ]
    in startLine : middleLines ++ [endLine]
  | otherwise = error ("'Range' has invalid positions: " ++ show r)

splitRanges :: Ranges -> HashMap Line [SingleLineRange]
splitRanges ranges =
  HashMap.fromListWith
    (flip (<>))
    [ (singleLineRangeLine lr, [lr])
    | lr <- concatMap rangeToSingleLineRanges (rangesToList ranges)
    ]
