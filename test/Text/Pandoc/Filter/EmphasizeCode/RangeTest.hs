{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Pandoc.Filter.EmphasizeCode.RangeTest where

import qualified Data.HashMap.Strict                             as HashMap
import           Data.Maybe                                      (mapMaybe)
import           Data.Tuple                                      (swap)
import           Test.Tasty.Hspec

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range
import           Text.Pandoc.Filter.EmphasizeCode.Testing.Ranges

-- Defaulting to Int instead of (Ord a => a) here to avoid ambiguous type vars
testDisjoint :: ((Int, Int), (Int, Int)) -> Bool -> Expectation
testDisjoint ((s1, e1), (s2, e2)) expected =
  disjoint s1 e1 s2 e2 `shouldBe` expected

spec_disjoint = do
  it "is disjoint when zero ends are contained by the other" $ do
    let input = ((1, 2), (3, 9))
    testDisjoint input True
    testDisjoint (swap input) True
  it "is not disjoint when one end is contained by the other" $ do
    let input = ((1, 4), (3, 9))
    testDisjoint input False
    testDisjoint (swap input) False
  it "is not disjoint when one end equals the other (ends are inclusive!)" $ do
    let input = ((1, 3), (3, 9))
    testDisjoint input False
    testDisjoint (swap input) False
  it "is not disjoint when one range is completely contained by the other" $ do
    let input = ((1, 9), (3, 7))
    testDisjoint input False
    testDisjoint (swap input) False

makeSingleLineRanges :: [(Line, Column, Maybe Column)] -> [SingleLineRange]
makeSingleLineRanges = mapMaybe mkSingleLineRange'
  where
    mkSingleLineRange' (line', start, end) = mkSingleLineRange line' start end

spec_mkPosRanges = do
  it "accepts single-position range" $ do
    rs <- mkPosRanges' [((1, 1), (1, 1))]
    map rangeToTuples (rangesToList rs) `shouldBe` [((1, Just 1), (1, Just 1))]
  it "sorts position ranges" $ do
    rs <- mkPosRanges' [((1, 1), (1, 7)), ((4, 1), (7, 2)), ((1, 8), (3, 4))]
    map rangeToTuples (rangesToList rs) `shouldBe`
      [ ((1, Just 1), (1, Just 7)) , ((1, Just 8), (3, Just 4)) , ((4, Just 1), (7, Just 2)) ]
  it "does not accept empty position ranges" $ do mkPosRanges' [] `shouldThrow` anyException

-- TODO(jez) Test splitRange on LineRange's, not just PosRange's
spec_splitRanges = do
  it "splits one range into line ranges" $ do
    rs <- mkPosRanges' [((1, 1), (1, 7))]
    let lrs = HashMap.fromList [(1, makeSingleLineRanges [(1, 1, Just 7)])]
    splitRanges rs `shouldBe` lrs
  it "splits two ranges into line ranges" $ do
    rs <- mkPosRanges' [((1, 1), (1, 7)), ((1, 8), (2, 2))]
    let lrs =
          HashMap.fromList
            [ (1, makeSingleLineRanges [(1, 1, Just 7), (1, 8, Nothing)])
            , (2, makeSingleLineRanges [(2, 1, Just 2)])
            ]
    splitRanges rs `shouldBe` lrs
  it "splits multiple ranges into line ranges" $ do
    rs <- mkPosRanges' [((1, 1), (1, 7)), ((1, 8), (3, 4)), ((8, 2), (10, 2))]
    let lrs =
          HashMap.fromList
            [ (1, makeSingleLineRanges [(1, 1, Just 7), (1, 8, Nothing)])
            , (2, makeSingleLineRanges [(2, 1, Nothing)])
            , (3, makeSingleLineRanges [(3, 1, Just 4)])
            , (8, makeSingleLineRanges [(8, 2, Nothing)])
            , (9, makeSingleLineRanges [(9, 1, Nothing)])
            , (10, makeSingleLineRanges [(10, 1, Just 2)])
            ]
    splitRanges rs `shouldBe` lrs

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
