{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Chunking
  ( LineChunk(..)
  , EmphasizedLine
  , EmphasizedLines
  , emphasizeRanges
  ) where

import           Data.HashMap.Strict                       (HashMap)
import qualified Data.HashMap.Strict                       as HashMap
import           Data.List                                 (foldl')
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

data LineChunk
  = Literal Text
  | Emphasized Text
  deriving (Show, Eq)

chunkText :: LineChunk -> Text
chunkText (Literal t)    = t
chunkText (Emphasized t) = t

type EmphasizedLine = [LineChunk]

type EmphasizedLines = [EmphasizedLine]

emphasizeRanges :: HashMap Line [LineRange] -> Text -> EmphasizedLines
emphasizeRanges ranges contents = zipWith chunkLine (Text.lines contents) [1 ..]
  where
    chunkLine line' lineNr =
      let (rest, _, chunks) =
            foldl'
              chunkRange
              (line', 0, [])
              (HashMap.lookupDefault [] lineNr ranges)
      in filter (not . Text.null . chunkText) (chunks ++ [Literal rest])
    chunkRange ::
         (Text, Column, EmphasizedLine)
      -> LineRange
      -> (Text, Column, EmphasizedLine)
    chunkRange (lineText, offset, chunks) r =
      case Text.splitAt startIndex lineText of
        (before, rest) ->
          case lineRangeEnd r of
            Just end ->
              let endIndex = fromIntegral (end - offset) - Text.length before
                  (emph, rest') = Text.splitAt endIndex rest
                  newOffset =
                    offset +
                    fromIntegral (Text.length lineText - Text.length rest')
              in (rest', newOffset, chunks ++ [Literal before, Emphasized emph])
            Nothing ->
              ( ""
              , offset + fromIntegral (Text.length lineText)
              , chunks ++ [Literal before, Emphasized rest])
      where
        startIndex = fromIntegral (lineRangeStart r - 1 - offset)
