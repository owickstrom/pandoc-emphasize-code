{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Latex
  ( Latex (Latex),
  )
where

#if MIN_VERSION_base(4,8,0)
import           Data.Semigroup                              ((<>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Pandoc.Definition as Pandoc
import Text.Pandoc.Filter.EmphasizeCode.Chunking
import Text.Pandoc.Filter.EmphasizeCode.Range
import Text.Pandoc.Filter.EmphasizeCode.Renderable

data Latex
  = Latex

escaped :: Char -> Text
escaped = \case
  '\\' -> "\\textbackslash{}"
  '#' -> "\\#"
  '&' -> "\\&"
  '{' -> "\\{"
  '}' -> "\\}"
  '$' -> "\\$"
  '_' -> "\\_"
  '%' -> "\\%"
  '¶' -> "\\P"
  '§' -> "\\S"
  '£' -> "\\£"
  '\'' -> "\\textquotesingle{}"
  '<' -> "\\textless{}"
  '>' -> "\\textgreater{}"
  c -> Text.singleton c

instance Renderable Latex where
  renderEmphasized _ (_, classes, _) lines' =
    Pandoc.RawBlock
      (Pandoc.Format "latex")
      (encloseInVerbatim emphasized)
    where
      languageAttr =
        case classes of
          [lang] -> ",language=" <> lang
          _ -> ""
      encloseInTextIt style t
        | Text.null t = t
        | otherwise =
          case style of
            Inline -> "£\\CodeEmphasis{" <> t <> "}£"
            Block -> "£\\CodeEmphasisLine{" <> t <> "}£"
      emphasizeNonSpace style t
        | Text.null t = t
        | otherwise =
          let (nonSpace, rest) = Text.break isSpace t
              (spaces, rest') = Text.span isSpace rest
           in mconcat
                [ encloseInTextIt style nonSpace,
                  spaces,
                  emphasizeNonSpace style rest'
                ]
      emphasizeChunk chunk =
        case chunk of
          Literal t -> t
          Emphasized style t -> emphasizeNonSpace style (Text.concatMap escaped t)
      emphasized = Text.unlines (map (foldMap emphasizeChunk) lines')
      encloseInVerbatim t =
        mconcat
          [ "\\begin{lstlisting}[escapechar=£",
            languageAttr,
            "]\n",
            t,
            "\\end{lstlisting}\n"
          ]
