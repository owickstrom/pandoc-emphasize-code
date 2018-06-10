{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Parser
  ( Parser
  , ParseError(..)
  , parseRanges
  , runParser
  ) where

import           Control.Monad.Except
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Text.Read                                 (readMaybe)

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

type Parser a = ExceptT ParseError Maybe a

data ParseError
  = InvalidRange Position
                 Position
  | InvalidRanges RangesError
  | InvalidRangeFormat Text
  | InvalidPosition Line
                    Column
  | InvalidPositionFormat Text
  | InvalidLineNumber Text
  | InvalidColumnNumber Text
  deriving (Show, Eq)

parseMaybe :: Read a => Text -> (Text -> ParseError) -> Parser a
parseMaybe t mkError =
  case readMaybe (Text.unpack t) of
    Just x  -> pure x
    Nothing -> throwError (mkError t)

split2 :: MonadError e m => Text -> Text -> (Text -> e) -> m (Text, Text)
split2 sep t err =
  case Text.splitOn sep t of
    [before, after] -> return (before, after)
    _               -> throwError (err t)

parsePosition :: Text -> Parser Position
parsePosition t = do
  (line, col) <- split2 ":" t InvalidPositionFormat
  line' <- Line <$> parseMaybe line InvalidLineNumber
  col' <- Column <$> parseMaybe col InvalidColumnNumber
  case mkPosition line' col' of
    Just position -> pure position
    Nothing       -> throwError (InvalidPosition line' col')

parseRange :: Text -> Parser Range
parseRange t = do
  (startStr, endStr) <- split2 "-" t InvalidRangeFormat
  start <- parsePosition startStr
  end <- parsePosition endStr
  case mkRange start end of
    Just range -> pure range
    Nothing    -> throwError (InvalidRange start end)

parseRanges :: Text -> Parser Ranges
parseRanges t = do
  let strs = filter (not . Text.null) (map Text.strip (Text.splitOn "," t))
  rs <- mapM parseRange strs
  case mkRanges rs of
    Left err     -> throwError (InvalidRanges err)
    Right ranges -> pure ranges

runParser :: Parser a -> Maybe (Either ParseError a)
runParser = runExceptT
