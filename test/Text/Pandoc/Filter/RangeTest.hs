module Text.Pandoc.Filter.RangeTest where

import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HashMap
import           Data.Maybe                        (mapMaybe)
import           Test.Tasty
import           Test.Tasty.Hspec

import           Text.Pandoc.Filter.Position
import           Text.Pandoc.Filter.Range
import           Text.Pandoc.Filter.Testing.Ranges

makeLineRanges :: [(Line, Column, Maybe Column)] -> [LineRange]
makeLineRanges = mapMaybe mkLineRange'
  where
    mkLineRange' (line, start, end) = mkLineRange line start end

spec_mkRanges =
  it "sorts ranges" $ do
    rs <- mkRanges' [((1, 1), (1, 7)), ((4, 1), (7, 2)), ((1, 8), (3, 4))]
    map rangeToTuples (rangesToList rs) `shouldBe`
      [((1, 1), (1, 7)), ((1, 8), (3, 4)), ((4, 1), (7, 2))]

spec_splitRanges = do
  it "splits one range into line ranges" $ do
    rs <- mkRanges' [((1, 1), (1, 7))]
    let lrs = HashMap.fromList [(1, makeLineRanges [(1, 1, Just 7)])]
    splitRanges rs `shouldBe` lrs
  it "splits two ranges into line ranges" $ do
    rs <- mkRanges' [((1, 1), (1, 7)), ((1, 8), (2, 2))]
    let lrs =
          HashMap.fromList
            [ (1, makeLineRanges [(1, 1, Just 7), (1, 8, Nothing)])
            , (2, makeLineRanges [(2, 1, Just 2)])
            ]
    splitRanges rs `shouldBe` lrs
  it "splits multiple ranges into line ranges" $ do
    rs <- mkRanges' [((1, 1), (1, 7)), ((1, 8), (3, 4)), ((8, 2), (10, 2))]
    let lrs =
          HashMap.fromList
            [ (1, makeLineRanges [(1, 1, Just 7), (1, 8, Nothing)])
            , (2, makeLineRanges [(2, 1, Nothing)])
            , (3, makeLineRanges [(3, 1, Just 4)])
            , (8, makeLineRanges [(8, 2, Nothing)])
            , (9, makeLineRanges [(9, 1, Nothing)])
            , (10, makeLineRanges [(10, 1, Just 2)])
            ]
    splitRanges rs `shouldBe` lrs

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
