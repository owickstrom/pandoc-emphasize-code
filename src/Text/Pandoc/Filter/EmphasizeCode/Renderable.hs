module Text.Pandoc.Filter.EmphasizeCode.Renderable where

import qualified Text.Pandoc.JSON                          as Pandoc

import           Text.Pandoc.Filter.EmphasizeCode.Chunking

class Renderable r where
  renderEmphasized :: r -> Pandoc.Attr -> EmphasizedLines -> Pandoc.Block
