{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Pandoc.Filter.EmphasizeCodeTest where

import           Test.Tasty.Hspec

import qualified Text.Pandoc.Filter.EmphasizeCode as Filter
import           Text.Pandoc.JSON

singleRangeHtmlMark :: Block
singleRangeHtmlMark =
  RawBlock
    "html"
    (mconcat
       [ "<pre class=\"my-lang\"><code>hello world\n"
       , "hej <mark class=\"inline\">världen</mark>\n"
       , "<mark class=\"inline\">hallo</mark> welt\n"
       , "hei verden</code></pre>"
       ])

singleRangeHtmlEm :: Block
singleRangeHtmlEm =
  RawBlock
    "html"
    (mconcat
       [ "<pre class=\"my-lang\"><code>hello world\n"
       , "hej <em class=\"inline\">världen</em>\n"
       , "<em class=\"inline\">hallo</em> welt\n"
       , "hei verden</code></pre>"
       ])

emphasizeCode :: Format -> String -> IO Block
emphasizeCode format ranges =
  Filter.emphasizeCode
    (Just format)
    (CodeBlock
       ("", ["my-lang"], [("emphasize", ranges)])
       "hello world\nhej världen\nhallo welt\nhei verden")

spec_emphasizeCode = do
  it "emphasizes HTML and a single position range" $
    emphasizeCode "html5" "2:5-3:5" `shouldReturn` singleRangeHtmlMark
  it "emphasizes HTML and a single position range over multiple lines" $
    emphasizeCode "html5" "2:5-4:3" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world\n"
         , "hej <mark class=\"inline\">världen</mark>\n"
         , "<mark class=\"inline\">hallo welt</mark>\n"
         , "<mark class=\"inline\">hei</mark> verden</code></pre>"
         ])
  it "emphasizes HTML and a single line range" $
    emphasizeCode "html5" "2-3" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world\n"
         , "<mark class=\"block\">hej världen</mark>\n"
         , "<mark class=\"block\">hallo welt</mark>\n"
         , "hei verden</code></pre>"
         ])
  it "emphasizes HTML and multiple ranges" $
    emphasizeCode "html5" "1:1-1:5,2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code><mark class=\"inline\">hello</mark> world\n"
         , "hej <mark class=\"inline\">världen</mark>\n"
         , "<mark class=\"inline\">hallo</mark> welt\n"
         , "hei verden</code></pre>"
         ])
  it "emphasizes HTML and multiple ranges (mixing range types)" $
    emphasizeCode "html5" "1-1,2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code><mark class=\"block\">hello world</mark>\n"
         , "hej <mark class=\"inline\">världen</mark>\n"
         , "<mark class=\"inline\">hallo</mark> welt\n"
         , "hei verden</code></pre>"
         ])
  it "emphasizes RevealJS HTML using <mark>" $
    emphasizeCode "revealjs" "2:5-3:5" `shouldReturn` singleRangeHtmlMark
  it "emphasizes HTML4 using <em>" $
    emphasizeCode "html" "2:5-3:5" `shouldReturn` singleRangeHtmlEm
  it "emphasizes markdown_github using <em>" $
    emphasizeCode "markdown_github" "2:5-3:5" `shouldReturn` singleRangeHtmlEm
  it "emphasizes latex and multiple ranges" $
    emphasizeCode "latex" "1:1-1:5,2:5-3:5" `shouldReturn`
    RawBlock
      "latex"
      (mconcat
         [ "\\begin{lstlisting}[escapechar=£,language=my-lang]\n"
         , "£\\CodeEmphasis{hello}£ world\n"
         , "hej £\\CodeEmphasis{världen}£\n"
         , "£\\CodeEmphasis{hallo}£ welt\n"
         , "hei verden\n"
         , "\\end{lstlisting}\n"
         ])

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
