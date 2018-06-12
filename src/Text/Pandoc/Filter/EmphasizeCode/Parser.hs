{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Parser
  ( Parser
  , ParseError(..)
  , parseRange
  , parseRanges
  , runParser
  ) where
#if MIN_VERSION_base(4,8,0)
import           Data.Semigroup                            ((<>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Control.Monad.Except
import qualified Data.List                                 as L
import           Data.List.NonEmpty
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Text.Read                                 (readMaybe)

import           Text.Pandoc.Filter.EmphasizeCode.Position
import           Text.Pandoc.Filter.EmphasizeCode.Range

type Parser a = ExceptT (NonEmpty ParseError) Maybe a

data ParseError
  = InvalidPosRange Position
                    Position
  | InvalidLineRange Line
                     Line
  | InvalidRanges RangesError
  | InvalidPosRangeFormat Text
  | InvalidLineRangeFormat Text
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
    Nothing -> throwError (pure (mkError t))

split2 :: MonadError e m => Text -> Text -> (Text -> e) -> m (Text, Text)
split2 sep t err =
  case Text.splitOn sep t of
    [before, after] -> return (before, after)
    _               -> throwError (err t)

parsePosition :: Text -> Parser Position
parsePosition t = do
  (line, col) <- split2 ":" t (pure . InvalidPositionFormat)
  line' <- Line <$> parseMaybe line InvalidLineNumber
  col' <- Column <$> parseMaybe col InvalidColumnNumber
  case mkPosition line' col' of
    Just position -> pure position
    Nothing       -> throwError (pure (InvalidPosition line' col'))

parsePosRange :: Text -> Parser PosRange
parsePosRange t = do
  (startStr, endStr) <- split2 "-" t (pure . InvalidPosRangeFormat)
  start <- parsePosition startStr
  end <- parsePosition endStr
  case mkPosRange start end of
    Just range -> pure range
    Nothing    -> throwError (pure (InvalidPosRange start end))

parseLineRange :: Text -> Parser LineRange
parseLineRange t = do
  (startStr, endStr) <- split2 "-" t (pure . InvalidLineRangeFormat)
  start <- Line <$> parseMaybe startStr InvalidLineNumber
  end <- Line <$> parseMaybe endStr InvalidLineNumber
  case mkLineRange start end of
    Just range -> pure range
    Nothing    -> throwError (pure (InvalidLineRange start end))

parseRange :: Text -> Parser Range
parseRange t =
  case runExceptT (parsePosRange t) of
    Just (Right pr) -> pure (PR pr)
    Just (Left err1) ->
      case runExceptT (parseLineRange t) of
        Just (Right lr)  -> pure (LR lr)
        Just (Left err2) -> throwError (err1 <> err2)
        Nothing          -> lift Nothing
    Nothing ->
      case runExceptT (parseLineRange t) of
        Just (Right lr)  -> pure (LR lr)
        Just (Left err2) -> throwError err2
        Nothing          -> lift Nothing

parseRanges :: Text -> Parser Ranges
parseRanges t = do
  let strs = L.filter (not . Text.null) (Text.strip <$> Text.splitOn "," t)
  rs <- mapM parseRange strs
  case mkRanges rs of
    Left err     -> throwError (pure (InvalidRanges err))
    Right ranges -> pure ranges

runParser :: Parser a -> Maybe (Either (NonEmpty ParseError) a)
runParser = runExceptT
