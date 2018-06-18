module Text.Pandoc.Filter.EmphasizeCode.Testing.Ranges where

import           Data.Maybe                                (mapMaybe)

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

mkPosRange' :: ((Line, Column), (Line, Column)) -> IO PosRange
mkPosRange' rcrc = do
  case makePosRange rcrc of
    Nothing -> fail "Invalid PosRange in test case"
    Just pr -> return pr
  where
    makePosRange ((r1, c1), (r2, c2)) = do
      p1 <- mkPosition r1 c1
      p2 <- mkPosition r2 c2
      mkPosRange p1 p2

mkPosRanges' :: [((Line, Column), (Line, Column))] -> IO Ranges
mkPosRanges' rs = do
  posRanges <- map PR <$> mapM mkPosRange' rs
  either (fail . show) return (mkRanges posRanges)

mkLineRange' :: (Line, Line) -> IO LineRange
mkLineRange' (l1, l2) = do
  case mkLineRange l1 l2 of
    Nothing -> fail "Invalid LineRange in test case"
    Just pr -> return pr

mkLineRanges' :: [(Line, Line)] -> IO Ranges
mkLineRanges' rs = do
  lineRanges <- map LR <$> mapM mkLineRange' rs
  either (fail . show) return (mkRanges lineRanges)

mkRanges' :: [((Line, Column), (Line, Column))] -> [(Line, Line)] -> IO Ranges
mkRanges' prs lrs = do
  posRanges <- map PR <$> mapM mkPosRange' prs
  lineRanges <- map LR <$> mapM mkLineRange' lrs
  either (fail . show) return (mkRanges $ posRanges ++ lineRanges)
