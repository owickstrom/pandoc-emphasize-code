{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Pretty where
#if MIN_VERSION_base(4,8,0)
import           Data.Semigroup                            ((<>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text

import           Text.Pandoc.Filter.EmphasizeCode.Parser
import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

printLine :: Line -> Text
printLine (Line n) = Text.pack (show n)

printColumn :: Column -> Text
printColumn (Column n) = Text.pack (show n)

printPosition :: Position -> Text
printPosition p = printLine (line p) <> ":" <> printColumn (column p)

printRange :: Range -> Text
printRange (PR pr) =
  printPosition (posRangeStart pr) <> "-" <> printPosition (posRangeEnd pr)
printRange (LR lr) =
  printLine (lineRangeStart lr) <> "-" <> printLine (lineRangeEnd lr)

printRangesError :: RangesError -> Text
printRangesError err =
  case err of
    EmptyRanges   -> "At least one range is required"
    Overlap r1 r2 -> printRange r1 <> " overlaps with " <> printRange r2

printParseError :: ParseError -> Text
printParseError err =
  case err of
    InvalidPosRange start end ->
      "Invalid range: " <> printPosition start <> " to " <> printPosition end
    InvalidRanges rangesErr -> printRangesError rangesErr
    InvalidPosRangeFormat t -> "Invalid range: " <> t
    InvalidPosition line' column' ->
      "Invalid position: " <> printLine line' <> " to " <> printColumn column'
    InvalidPositionFormat t -> "Invalid position: " <> t
    InvalidLineNumber n -> "Invalid line number: " <> n
    InvalidColumnNumber n -> "Invalid column number: " <> n
