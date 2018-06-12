{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Pandoc.Filter.EmphasizeCode.ChunkingTest where

import           Test.Tasty.Hspec

import           Text.Pandoc.Filter.EmphasizeCode.Chunking
import           Text.Pandoc.Filter.EmphasizeCode.Range
import           Text.Pandoc.Filter.EmphasizeCode.Testing.Ranges

spec_emphasizeRanges = do
  it "emphasizes a single line range" $ do
    rs <- splitRanges <$> mkPosRanges' [((1, 1), (1, 7))]
    emphasizeRanges rs "hello world" `shouldBe`
      [[Emphasized Inline "hello w", Literal "orld"]]
  it "emphasizes multiple line ranges on a single line" $ do
    rs <- splitRanges <$> mkPosRanges' [((1, 1), (1, 3)), ((1, 5), (1, 8))]
    emphasizeRanges rs "hello world" `shouldBe`
      [ [ Emphasized Inline "hel"
        , Literal "l"
        , Emphasized Inline "o wo"
        , Literal "rld"
        ]
      ]
  it "emphasizes multiple line ranges on multiple lines" $ do
    rs <-
      splitRanges <$>
      mkPosRanges' [((1, 1), (1, 5)), ((1, 7), (2, 3)), ((4, 5), (4, 10))]
    emphasizeRanges rs "hello world\nhej världen\nhallo welt\nhei verden" `shouldBe`
      [ [Emphasized Inline "hello", Literal " ", Emphasized Inline "world"]
      , [Emphasized Inline "hej", Literal " världen"]
      , [Literal "hallo welt"]
      , [Literal "hei ", Emphasized Inline "verden"]
      ]

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
