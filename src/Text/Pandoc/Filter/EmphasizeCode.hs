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
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Pandoc.JSON
import Text.Read (readMaybe)

import Text.Pandoc.Filter.Range

data MissingRangePart
  = Start
  | End
  deriving (Show, Eq)

data EmphasisError
  = InvalidRange Position
                 Position
  | InvalidRangeFormat Text
  | InvalidPosition Int
                    Int
  | InvalidPositionFormat Text
  deriving (Show, Eq)

parseRange :: HashMap String String -> ExceptT EmphasisError Maybe Range
parseRange attrs = do
  emphasize <- lift (HM.lookup "emphasize" attrs)
  parseRange (Text.pack emphasize)
  where
    split2 sep t err =
      case Text.splitOn sep t of
        [before, after] -> return (before, after)
        _ -> throwError (err t)
    parseInt = lift . readMaybe . Text.unpack
    parsePosition t = do
      (row, col) <- split2 ":" t InvalidPositionFormat
      row' <- parseInt row
      col' <- parseInt col
      lift (mkPosition row' col')
    parseRange t = do
      (startStr, endStr) <- split2 "-" t InvalidRangeFormat
      start <- parsePosition startStr
      end <- parsePosition endStr
      lift (mkRange start end)

filterAttributes :: [(String, String)] -> [(String, String)]
filterAttributes = filter nonFilterAttribute
  where
    nonFilterAttribute (key, _) = key `notElem` attributeNames
    attributeNames = ["emphasize"]

printAndFail :: EmphasisError -> IO Block
printAndFail = fail . formatError
  where
    formatError =
      \case
        InvalidRange start end ->
          "Invalid range: " ++ show start ++ " to " ++ show end

emphasizeRangeHtml ::
     (String, [String], [(String, String)]) -> Text -> Range -> Block
emphasizeRangeHtml (_, classes, _) t range =
  RawBlock (Format "html") (Text.unpack (encloseInPreCode classes emphasized))
  where
    start = rangeStart range
    end = rangeEnd range
    injectAtColumn col sep line =
      let (before, after) = Text.splitAt col line
      in before <> sep <> after
    addEmphasis line lineNr
      | lineNr == positionRow start =
        injectAtColumn (positionColumn start - 1) "<em>" line
      | lineNr == positionRow end =
        injectAtColumn (positionColumn end) "</em>" line
      | otherwise = line
    emphasized = Text.unlines (zipWith addEmphasis (Text.lines t) [1 ..])
    encloseInPreCode classes t =
      mconcat
        [ "<pre class="
        , Text.pack (unwords classes)
        , ">"
        , "<code>"
        , t
        , "</code>"
        , "</pre>"
        ]

data LatexMode
  = Latex
  | Beamer

emphasizeRangeLatex ::
     LatexMode
  -> (String, [String], [(String, String)])
  -> Text
  -> Range
  -> Block
emphasizeRangeLatex mode (_, classes, _) t range =
  RawBlock (Format "latex") (Text.unpack (encloseInVerbatim emphasized))
  where
    start = rangeStart range
    end = rangeEnd range
    encloseInTextIt t
      | Text.null t = t
      | otherwise =
        case mode of
          Latex -> "@\\textbf{\\textcolor{PandocEmphasizeCodeColor}{" <> t <> "}}@"
          Beamer -> "\\EmphasisTok{" <> t <> "}"
    emphasizeNonSpace t
      | Text.null t = t
      | otherwise =
        let (nonSpace, rest) = Text.break isSpace t
            (spaces, rest') = Text.span isSpace rest
        in mconcat [encloseInTextIt nonSpace, spaces, emphasizeNonSpace rest']
    addEmphasis line lineNr
      | lineNr == positionRow start =
        let (before, after) = Text.splitAt (positionColumn start - 1) line
        in before <> emphasizeNonSpace after
      | lineNr == positionRow end =
        let (before, after) = Text.splitAt (positionColumn end) line
        in emphasizeNonSpace before <> after
      | lineIntersectsWithRange lineNr range = emphasizeNonSpace line
      | otherwise = line
    emphasized = Text.unlines (zipWith addEmphasis (Text.lines t) [1 ..])
    encloseInVerbatim t =
      case mode of
        Latex ->
          Text.unlines
            ["\\begin{lstlisting}[escapechar=@]", t, "\\end{lstlisting}"]
        Beamer ->
          Text.unlines
            [ "\\begin{Shaded}"
            , "\\begin{Highlighting}[]"
            , t
            , "\\end{Highlighting}"
            , "\\end{Shaded}"
            ]

type Emphasizer
   = (String, [String], [(String, String)]) -> Text -> Range -> Block

asEmphasizer :: Format -> Maybe Emphasizer
asEmphasizer f
  | f `elem` ["html", "html5"] = Just emphasizeRangeHtml
  | f == "latex" = Just (emphasizeRangeLatex Latex)
  | f == "beamer" = Just (emphasizeRangeLatex Beamer)
  | otherwise = Nothing

-- | A Pandoc filter that emphasizes code sections.
emphasizeCode :: Maybe Format -> Block -> IO Block
emphasizeCode (Just (asEmphasizer -> Just emphasizer)) cb@(CodeBlock h@(id', classes, attrs) contents) =
  case runExceptT (parseRange (HM.fromList attrs)) of
    Just (Right range) -> return (emphasizer h (Text.pack contents) range)
    Just (Left err) -> printAndFail err
    Nothing -> return cb
emphasizeCode _ x = return x
