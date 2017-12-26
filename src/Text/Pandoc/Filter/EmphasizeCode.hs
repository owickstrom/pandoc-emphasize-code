{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Text.Pandoc.Filter.EmphasizeCode
  ( emphasizeCode
  ) where
#if MIN_VERSION_base(4,8,0)
import           Data.Semigroup                            ((<>))
#else
import           Control.Applicative
import           Data.Monoid
#endif
import           Data.Char                                 (isSpace)
import qualified Data.HashMap.Strict                       as HM
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Text.Pandoc.JSON

import           Text.Pandoc.Filter.EmphasizeCode.Chunking
import           Text.Pandoc.Filter.EmphasizeCode.Parser
import           Text.Pandoc.Filter.EmphasizeCode.Pretty
import           Text.Pandoc.Filter.EmphasizeCode.Range

printAndFail :: ParseError -> IO a
printAndFail = fail . Text.unpack . printParseError

emphasizeChunkHtml :: LineChunk -> Text
emphasizeChunkHtml chunk =
  case chunk of
    Literal t    -> t
    Emphasized t -> "<em>" <> t <> "</em>"

emphasizeRangeHtml ::
     (String, [String], [(String, String)]) -> EmphasizedLines -> Block
emphasizeRangeHtml (_, classes, _) lines' =
  RawBlock (Format "html") (Text.unpack emphasized)
  where
    classAttr =
      if null classes
        then ""
        else " class=\"" <> Text.pack (unwords classes) <> "\""
    emphasized =
      mconcat
        [ "<pre"
        , classAttr
        , "><code>"
        , Text.dropEnd
            1
            (Text.unlines (map (foldMap emphasizeChunkHtml) lines'))
        , "</code>"
        , "</pre>"
        ]

emphasizeRangeMarkdown ::
     (String, [String], [(String, String)]) -> EmphasizedLines -> Block
emphasizeRangeMarkdown (_, classes, _) lines' =
  RawBlock (Format "html") (Text.unpack emphasized)
  where
    classAttr =
      if null classes
        then ""
        else " class=\"" <> Text.pack (unwords classes) <> "\""
    emphasized =
      mconcat
        [ "<pre"
        , classAttr
        , "><code>"
        , Text.dropEnd
            1
            (Text.unlines (map (foldMap emphasizeChunkHtml) lines'))
        , "</code>"
        , "</pre>"
        ]

emphasizeRangeLatex ::
     (String, [String], [(String, String)]) -> EmphasizedLines -> Block
emphasizeRangeLatex (_, classes, _) lines' =
  RawBlock (Format "latex") (Text.unpack (encloseInVerbatim emphasized))
  where
    languageAttr =
      case classes of
        [lang] -> ",language=" <> Text.pack lang
        _      -> ""
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
        Literal t    -> t
        Emphasized t -> emphasizeNonSpace t
    emphasized = Text.unlines (map (foldMap emphasizeChunk) lines')
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

lookupRanges :: HM.HashMap String String -> Maybe Text.Text
lookupRanges attrs = Text.pack <$> HM.lookup "emphasize" attrs

-- | A Pandoc filter that emphasizes code blocks.
emphasizeCode :: Maybe Format -> Block -> IO Block
emphasizeCode (Just (asEmphasizer -> Just emphasizer)) cb@(CodeBlock (id', classes, attrs) contents) =
  case lookupRanges attrs' >>= (runParser . parseRanges) of
    Just (Right ranges) ->
      let lines' = emphasizeRanges (splitRanges ranges) (Text.pack contents)
      in return
           (emphasizer
              (id', classes, HM.toList (HM.delete "emphasize" attrs'))
              lines')
    Just (Left err) -> printAndFail err
    Nothing -> return cb
  where
    attrs' = HM.fromList attrs
emphasizeCode _ x = return x
