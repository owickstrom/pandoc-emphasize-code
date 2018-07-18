{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Filter.EmphasizeCode.Html
  ( EmphasisTag(..)
  , Html(Html)
  ) where

import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Lucid as Html
import qualified Text.Pandoc.Definition as Pandoc

import Text.Pandoc.Filter.EmphasizeCode.Chunking
import Text.Pandoc.Filter.EmphasizeCode.Range
import Text.Pandoc.Filter.EmphasizeCode.Renderable

data EmphasisTag
  = Em
  | Mark

newtype Html =
  Html EmphasisTag

styleClass :: EmphasisStyle -> Html.Attribute
styleClass Inline = Html.class_ "inline"
styleClass Block = Html.class_ "block"

emphasisElement ::
     EmphasisTag -> [Html.Attribute] -> Html.Html () -> Html.Html ()
emphasisElement Em = Html.em_
emphasisElement Mark = Html.mark_

emphasizeChunkHtml :: EmphasisTag -> LineChunk -> Html.Html ()
emphasizeChunkHtml tag chunk =
  case chunk of
    Literal t -> Html.toHtml t
    Emphasized style t -> emphasisElement tag [styleClass style] (Html.toHtml t)

instance Renderable Html where
  renderEmphasized (Html tag) (_, classes, _) lines' =
    Pandoc.RawBlock
      (Pandoc.Format "html")
      (TextLazy.unpack (Html.renderText emphasized))
    where
      classAttrs =
        if null classes
          then []
          else [Html.class_ (Text.pack (unwords classes))]
      emphasized =
        Html.pre_ classAttrs $
        Html.code_ $
        mconcat
          (intersperse
             (Html.toHtmlRaw ("\n" :: Text.Text))
             (map (foldMap (emphasizeChunkHtml tag)) lines'))
