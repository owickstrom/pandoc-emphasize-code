{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.ChunkingTest where

import           Data.HashMap.Strict               (HashMap)
import qualified Data.HashMap.Strict               as HashMap
import           Test.Tasty
import           Test.Tasty.Hspec

import           Text.Pandoc.Filter.Chunking
import           Text.Pandoc.Filter.Position
import           Text.Pandoc.Filter.Range
import           Text.Pandoc.Filter.Testing.Ranges

spec_emphasizeRanges = do
  it "emphasizes a single line range" $ do
    rs <- splitRanges <$> mkRanges' [((1, 1), (1, 7))]
    emphasizeRanges rs "hello world" `shouldBe`
      [[Emphasized "hello w", Literal "orld"]]
  it "emphasizes multiple line ranges on a single line" $ do
    rs <- splitRanges <$> mkRanges' [((1, 1), (1, 3)), ((1, 5), (1, 8))]
    emphasizeRanges rs "hello world" `shouldBe`
      [[Emphasized "hel", Literal "l", Emphasized "o wo", Literal "rld"]]
  it "emphasizes multiple line ranges on multiple lines" $ do
    rs <-
      splitRanges <$>
      mkRanges' [((1, 1), (1, 5)), ((1, 7), (2, 3)), ((4, 5), (4, 10))]
    emphasizeRanges rs "hello world\nhej världen\nhallo welt\nhei verden" `shouldBe`
      [ [Emphasized "hello", Literal " ", Emphasized "world"]
      , [Emphasized "hej", Literal " världen"]
      , [Literal "hallo welt"]
      , [Literal "hei ", Emphasized "verden"]
      ]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
