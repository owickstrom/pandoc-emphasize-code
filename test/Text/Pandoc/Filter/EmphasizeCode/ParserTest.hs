{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Pandoc.Filter.EmphasizeCode.ParserTest where

import           Control.Monad                                   (forM_)
import           Data.List.NonEmpty
import qualified Data.Text                                       as T

import           Test.Tasty.Hspec
import           Text.Pandoc.Filter.EmphasizeCode.Parser
import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range
import           Text.Pandoc.Filter.EmphasizeCode.Testing.Ranges

spec_parseRangeOk = do
  it "parses a simple PosRange" $ do
    r <- PR <$> mkPosRange' ((2, 3), (2, 14))
    runParser (parseRange "2:3-2:14") `shouldBe` Just (Right r)
  it "parses a simple LineRange" $ do
    r <- LR <$> mkLineRange' (2, 3)
    runParser (parseRange "2-3") `shouldBe` Just (Right r)

errorTests :: [(T.Text, NonEmpty ParseError)]
errorTests =
  [ ("2;3-2:14", InvalidPositionFormat "2;3" :| [InvalidLineNumber "2;3"])
  , ("a:3-2:14", InvalidLineNumber "a" :| [InvalidLineNumber "a:3"])
  , ("2:z-2:14", InvalidColumnNumber "z" :| [InvalidLineNumber "2:z"])
  , ( "0:3-2:14"
    , InvalidPosition (Line 0) (Column 3) :| [InvalidLineNumber "0:3"])
  , ( "2:0-2:14"
    , InvalidPosition (Line 2) (Column 0) :| [InvalidLineNumber "2:0"])
  , ( "2:3 2:14"
    , InvalidPosRangeFormat "2:3 2:14" :| [InvalidLineRangeFormat "2:3 2:14"])
  ]

spec_parseRangeError = do
  forM_ errorTests $ \(input, expected) ->
    it (T.unpack input ++ " throws " ++ show expected) $ do
      runParser (parseRange input) `shouldBe` Just (Left expected)

isValid (Just (Right _)) = True
isValid _                = False

isInvalidPosRange :: Maybe (Either (NonEmpty ParseError) b) -> Bool
isInvalidPosRange (Just (Left (InvalidPosRange _ _ :| []))) = True
isInvalidPosRange _                                         = False

isInvalidPosRangeLineNumber :: Maybe (Either (NonEmpty ParseError) b) -> Bool
isInvalidPosRangeLineNumber x =
  case x of
    (Just (Left (InvalidPosRange _ _ :| [InvalidLineNumber _]))) -> True
    _                                                            -> False

errorPosRangeTests = ["2:3-1:1"]

spec_parseRangeInvalidPosRange = do
  forM_ errorPosRangeTests $ \input ->
    it (T.unpack input ++ " throws InvalidPosRange") $ do
      runParser (parseRange input) `shouldSatisfy` isInvalidPosRangeLineNumber

spec_parseRangeEdgeCases = do
  it "should handle single character position range" $ do
    runParser (parseRange "2:3-2:3") `shouldSatisfy` isValid
  it "LineRange end should be inclusive" $ do
    runParser (parseRange "1-1") `shouldSatisfy` isValid

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
