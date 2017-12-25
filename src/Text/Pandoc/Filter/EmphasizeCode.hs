{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Text.Pandoc.Filter.EmphasizeCode
  ( emphasizeCode
  ) where
#if MIN_VERSION_base(4,8,0)
import Data.Semigroup ((<>))
#else
import Control.Applicative
import Data.Monoid
#endif
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (isSpace)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl', isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Pandoc.JSON
import Text.Read (readMaybe)

import Text.Pandoc.Filter.Chunking
import Text.Pandoc.Filter.Position
import Text.Pandoc.Filter.Range

data MissingRangePart
  = Start
  | End
  deriving (Show, Eq)

data EmphasisError
  = InvalidRange Position
                 Position
  | InvalidRanges InvalidRanges
  | InvalidRangeFormat Text
  | InvalidPosition Line
                    Column
  | InvalidPositionFormat Text
  | InvalidLineNumber Text
  | InvalidColumnNumber Text
  deriving (Show, Eq)

parseMaybe :: Read a => Text -> (Text -> EmphasisError) -> ExceptT EmphasisError Maybe a
parseMaybe t mkError =
  case readMaybe (Text.unpack t) of
    Just x -> pure x
    Nothing -> throwError (mkError t)

parseRanges :: HashMap String String -> ExceptT EmphasisError Maybe Ranges
parseRanges attrs = do
  emphasize <- lift (HM.lookup "emphasize" attrs)
  parseRanges (Text.pack emphasize)
  where
    split2 sep t err =
      case Text.splitOn sep t of
        [before, after] -> return (before, after)
        _ -> throwError (err t)
    parsePosition t = do
      (line, col) <- split2 ":" t InvalidPositionFormat
      line' <- Line <$> parseMaybe line InvalidLineNumber
      col' <- Column <$> parseMaybe col InvalidColumnNumber
      case mkPosition line' col' of
        Just position -> pure position
        Nothing -> throwError (InvalidPosition line' col')
    parseRange t = do
      (startStr, endStr) <- split2 "-" t InvalidRangeFormat
      start <- parsePosition startStr
      end <- parsePosition endStr
      case mkRange start end of
        Just range -> pure range
        Nothing -> throwError (InvalidRange start end)
    parseRanges t = do
      let strs = filter (not . Text.null) (map Text.strip (Text.splitOn "," t))
      rs <- mapM parseRange strs
      case mkRanges rs of
        Left err -> throwError (InvalidRanges err)
        Right ranges -> pure ranges

filterAttributes :: [(String, String)] -> [(String, String)]
filterAttributes = filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["emphasize"]

printAndFail :: EmphasisError -> IO a
printAndFail = fail . formatError
  where
    formatError =
      \case
        InvalidRange start end ->
          "Invalid range: " ++ show start ++ " to " ++ show end
        err -> show err

emphasizeRangeHtml ::
     (String, [String], [(String, String)]) -> EmphasizedLines -> Block
emphasizeRangeHtml (_, classes, _) lines =
  RawBlock (Format "html") (Text.unpack emphasized)
  where
    emphasizeChunk chunk =
      case chunk of
        Literal t -> t
        Emphasized t -> "<em>" <> t <> "</em>"
    classAttr =
      if null classes
         then ""
         else " class=\"" <> Text.pack (unwords classes) <> "\""
    emphasized =
      mconcat
        [ "<pre"
        , classAttr
        , "><code>"
        , Text.dropEnd 1 (Text.unlines (map (foldMap emphasizeChunk) lines))
        , "</code>"
        , "</pre>"
        ]

emphasizeRangeMarkdown ::
     (String, [String], [(String, String)]) -> EmphasizedLines -> Block
emphasizeRangeMarkdown (_, classes, _) lines =
  RawBlock (Format "html") (Text.unpack emphasized)
  where
    emphasizeChunk chunk =
      case chunk of
        Literal t -> t
        Emphasized t -> "<em>" <> t <> "</em>"
    classAttr =
      if null classes
         then ""
         else " class=\"" <> Text.pack (unwords classes) <> "\""
    emphasized =
      mconcat
        [ "<pre"
        , classAttr
        , "><code>"
        , Text.dropEnd 1 (Text.unlines (map (foldMap emphasizeChunk) lines))
        , "</code>"
        , "</pre>"
        ]

emphasizeRangeLatex ::
  (String, [String], [(String, String)])
  -> EmphasizedLines
  -> Block
emphasizeRangeLatex (_, classes, _) lines =
  RawBlock (Format "latex") (Text.unpack (encloseInVerbatim emphasized))
  where
    languageAttr =
      case classes of
        [lang] -> ",language=" <> Text.pack lang
    encloseInTextIt t
      | Text.null t = t
      | otherwise = "£\\CodeEmphasis{" <> t <> "}£"
    emphasizeNonSpace t
      | Text.null t = t
      | otherwise =
        let (nonSpace, rest) = Text.break isSpace t
            (spaces, rest') = Text.span isSpace rest
        in mconcat [encloseInTextIt nonSpace, spaces, emphasizeNonSpace rest']
    emphasizeChunk chunk =
      case chunk of
        Literal t -> t
        Emphasized t -> emphasizeNonSpace t
    emphasized = Text.unlines (map (foldMap emphasizeChunk) lines)
    encloseInVerbatim t =
      mconcat
        [ "\\begin{lstlisting}[escapechar=£"
        , languageAttr
        , "]\n"
        , t
        , "\\end{lstlisting}\n"
        ]

type Emphasizer
   = (String, [String], [(String, String)]) -> EmphasizedLines -> Block

asEmphasizer :: Format -> Maybe Emphasizer
asEmphasizer f
  | f `elem` ["html", "html5"] = Just emphasizeRangeHtml
  | f == "markdown_github" = Just emphasizeRangeMarkdown
  | f == "latex" = Just emphasizeRangeLatex
  | f == "beamer" = Just emphasizeRangeLatex
  | otherwise = Nothing

-- | A Pandoc filter that emphasizes code sections.
emphasizeCode :: Maybe Format -> Block -> IO Block
emphasizeCode (Just (asEmphasizer -> Just emphasizer)) cb@(CodeBlock (id', classes, attrs) contents) =
  case runExceptT (parseRanges attrs') of
    Just (Right ranges) ->
      let lines = emphasizeRanges (splitRanges ranges) (Text.pack contents)
      in return
           (emphasizer
              (id', classes, HM.toList (HM.delete "emphasize" attrs'))
              lines)
    Just (Left err) -> printAndFail err
    Nothing -> return cb
  where
    attrs' = HM.fromList attrs
emphasizeCode _ x = return x
