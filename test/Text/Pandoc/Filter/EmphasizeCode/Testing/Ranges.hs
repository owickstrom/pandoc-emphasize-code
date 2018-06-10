module Text.Pandoc.Filter.EmphasizeCode.Testing.Ranges where

import           Data.Maybe                                (mapMaybe)

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

mkRanges' :: [((Line, Column), (Line, Column))] -> IO Ranges
mkRanges' rs = either (fail . show) return (mkRanges (mapMaybe makeRange rs))
  where
    makeRange ((r1, c1), (r2, c2)) = do
      p1 <- mkPosition r1 c1
      p2 <- mkPosition r2 c2
      mkRange p1 p2
