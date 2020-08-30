{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Text.Pandoc.Filter.EmphasizeCode
  ( emphasizeCode
  ) where

import qualified Data.HashMap.Strict                         as HM
import           Data.List.NonEmpty
import qualified Data.Text                                   as Text
import qualified Text.Pandoc.JSON                            as Pandoc

import           Text.Pandoc.Filter.EmphasizeCode.Chunking
import           Text.Pandoc.Filter.EmphasizeCode.Html
import           Text.Pandoc.Filter.EmphasizeCode.Latex
import           Text.Pandoc.Filter.EmphasizeCode.Parser
import           Text.Pandoc.Filter.EmphasizeCode.Pretty
import           Text.Pandoc.Filter.EmphasizeCode.Range
import           Text.Pandoc.Filter.EmphasizeCode.Renderable
import Data.Text (Text)

printAndFail :: NonEmpty ParseError -> IO a
printAndFail (err :| []) = fail . Text.unpack . printParseError $ err
printAndFail errs        = fail . Text.unpack . printParseErrors $ errs

toRenderer ::
     Pandoc.Format -> Maybe (Pandoc.Attr -> EmphasizedLines -> Pandoc.Block)
toRenderer f
  | f `elem` ["html", "markdown_github"] = Just (renderEmphasized (Html Em))
  | f `elem` ["html5", "revealjs"] = Just (renderEmphasized (Html Mark))
  | f == "latex" = Just (renderEmphasized Latex)
  | f == "beamer" = Just (renderEmphasized Latex)
  | otherwise = Nothing

lookupRanges :: HM.HashMap Text Text -> Maybe Text.Text
lookupRanges = HM.lookup "emphasize"

-- | A Pandoc filter that emphasizes code blocks.
emphasizeCode :: Maybe Pandoc.Format -> Pandoc.Block -> IO Pandoc.Block
emphasizeCode (Just (toRenderer -> Just render)) cb@(Pandoc.CodeBlock (id', classes, attrs) contents) =
  case lookupRanges attrs' >>= (runParser . parseRanges) of
    Just (Right ranges) ->
      let lines' = emphasizeRanges (splitRanges ranges) contents
          block =
            render
              (id', classes, HM.toList (HM.delete "emphasize" attrs'))
              lines'
      in return block
    Just (Left err) -> printAndFail err
    Nothing -> return cb
  where
    attrs' = HM.fromList attrs
emphasizeCode _ x = return x
