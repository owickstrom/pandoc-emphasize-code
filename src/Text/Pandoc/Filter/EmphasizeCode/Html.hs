module Text.Pandoc.Filter.EmphasizeCode.Html
  ( Html(Html)
  ) where

import Data.List (intersperse)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Lucid as Html
import qualified Text.Pandoc.Definition as Pandoc

import Text.Pandoc.Filter.EmphasizeCode.Chunking
import Text.Pandoc.Filter.EmphasizeCode.Renderable

data Html =
  Html

emphasizeChunkHtml :: LineChunk -> Html.Html ()
emphasizeChunkHtml chunk =
  case chunk of
    Literal t -> Html.toHtml t
    Emphasized t -> Html.em_ (Html.toHtml t)

instance Renderable Html where
  renderEmphasized _ (_, classes, _) lines' =
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
          (intersperse (Html.br_ []) (map (foldMap emphasizeChunkHtml) lines'))

