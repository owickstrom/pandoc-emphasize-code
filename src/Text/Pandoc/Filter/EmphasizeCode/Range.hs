{-# LANGUAGE CPP #-}

-- | Ranges that cannot be constructed with incorrect bounds.
module Text.Pandoc.Filter.EmphasizeCode.Range
  ( PosRange
  , mkPosRange
  , posRangeStart
  , posRangeEnd
  , posRangeToTuple
  , LineRange
  , mkLineRange
  , lineRangeStart
  , lineRangeEnd
  , lineRangeToTuple
  , Range(..)
  , rangeToTuples
  , disjoint
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
import           Control.Monad                             (foldM_)
import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HashMap
import           Data.List                                 (sortOn)

import           Text.Pandoc.Filter.EmphasizeCode.Position

data PosRange = PosRange
  { posRangeStart :: Position
  , posRangeEnd   :: Position
  } deriving (Eq, Show)

mkPosRange :: Position -> Position -> Maybe PosRange
mkPosRange s e
  | s <= e = Just (PosRange s e)
  | otherwise = Nothing

posRangeToTuple :: PosRange -> (Position, Position)
posRangeToTuple (PosRange p1 p2) = (p1, p2)

data LineRange = LineRange
  { lineRangeStart :: Line
  , lineRangeEnd   :: Line
  } deriving (Eq, Show)

mkLineRange :: Line -> Line -> Maybe LineRange
mkLineRange s e
  | s == 0 || e == 0 = Nothing
  | s <= e = Just (LineRange s e)
  | otherwise = Nothing

lineRangeToTuple :: LineRange -> (Line, Line)
lineRangeToTuple (LineRange l1 l2) = (l1, l2)

data Range
  = PR PosRange
  | LR LineRange
  deriving (Eq, Show)

wrapSndJust :: (a, b) -> (a, Maybe b)
wrapSndJust (x, y) = (x, Just y)

rangeToTuples :: Range -> ((Line, Maybe Column), (Line, Maybe Column))
rangeToTuples (PR pr) =
  let (p1, p2) = posRangeToTuple pr
  in (wrapSndJust $ positionToTuple p1, wrapSndJust $ positionToTuple p2)
rangeToTuples (LR lr) =
  let (l1, l2) = lineRangeToTuple lr
  in ((l1, Nothing), (l2, Nothing))

disjoint :: (Ord a) => a -> a -> a -> a -> Bool
disjoint s1 e1 s2 e2 = (e1 < s2) || (e2 < s1)

rangesAreDisjoint :: Range -> Range -> Bool
rangesAreDisjoint (PR (PosRange s1 e1)) (PR (PosRange s2 e2)) =
  disjoint s1 e1 s2 e2
rangesAreDisjoint (LR (LineRange s1 e1)) (LR (LineRange s2 e2)) =
  disjoint s1 e1 s2 e2
rangesAreDisjoint (LR (LineRange s1 e1)) (PR (PosRange s2 e2)) =
  let (s2l, _) = positionToTuple s2
      (e2l, _) = positionToTuple e2
  in disjoint s1 e1 s2l e2l
rangesAreDisjoint (PR pw) (LR lw)
  -- Flipping argument order doesn't affect whether the ranges are disjoint
 = rangesAreDisjoint (LR lw) (PR pw)

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

rangeStartPos :: Range -> Position
rangeStartPos (PR (PosRange s _)) = s
rangeStartPos (LR (LineRange s _)) =
  case mkPosition s 1 of
    Just sp -> sp
    -- Impossible: s is a valid line (mkLineRange) and 1 is a valid column
    Nothing -> error "rangeStartPos: failed to meet mkPosition invariant!"

mkRanges :: [Range] -> Either RangesError Ranges
mkRanges [] = Left EmptyRanges
mkRanges ranges = do
  let sorted = sortOn rangeStartPos ranges
  foldM_ checkOverlap Nothing sorted
  pure (Ranges sorted)
  where
    checkOverlap (Just last') this =
      if last' `rangesAreDisjoint` this
        then return (Just this)
        else Left (Overlap last' this)
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
rangeToSingleLineRanges (PR pr@(PosRange p1 p2))
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
  | otherwise = error ("'PosRange' has invalid positions: " ++ show pr)
rangeToSingleLineRanges (LR (LineRange l1 l2)) =
  [SingleLineRange n 1 Nothing | n <- [l1 .. l2]]

splitRanges :: Ranges -> HashMap Line [SingleLineRange]
splitRanges ranges =
  HashMap.fromListWith
    (flip (<>))
    [ (singleLineRangeLine lr, [lr])
    | lr <- concatMap rangeToSingleLineRanges (rangesToList ranges)
    ]
