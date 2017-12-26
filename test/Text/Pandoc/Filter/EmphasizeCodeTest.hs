{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Text.Pandoc.Filter.EmphasizeCodeTest where

import           Test.Tasty.Hspec

import qualified Text.Pandoc.Filter.EmphasizeCode as Filter
import           Text.Pandoc.JSON

emphasizeCode :: Format -> String -> IO Block
emphasizeCode format ranges =
  Filter.emphasizeCode
    (Just format)
    (CodeBlock
       ("", ["my-lang"], [("emphasize", ranges)])
       "hello world\nhej världen\nhallo welt\nhei verden")

spec_emphasizeCode = do
  it "emphasizes HTML and a single range" $
    emphasizeCode "html5" "2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world<br>"
         , "hej <mark>världen</mark><br>"
         , "<mark>hallo</mark> welt<br>"
         , "hei verden</code></pre>"
         ])
  it "emphasizes HTML and a single range over multiple lines" $
    emphasizeCode "html5" "2:5-4:3" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world<br>"
         , "hej <mark>världen</mark><br>"
         , "<mark>hallo welt</mark><br>"
         , "<mark>hei</mark> verden</code></pre>"
         ])
  it "emphasizes HTML and multiple ranges" $
    emphasizeCode "html5" "1:1-1:5,2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code><mark>hello</mark> world<br>"
         , "hej <mark>världen</mark><br>"
         , "<mark>hallo</mark> welt<br>"
         , "hei verden</code></pre>"
         ])
  it "emphasizes HTML4 using <em>" $
    emphasizeCode "html" "2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world<br>"
         , "hej <em>världen</em><br>"
         , "<em>hallo</em> welt<br>"
         , "hei verden</code></pre>"
         ])
  it "emphasizes markdown_github using <em>" $
    emphasizeCode "markdown_github" "2:5-3:5" `shouldReturn`
    RawBlock
      "html"
      (mconcat
         [ "<pre class=\"my-lang\"><code>hello world<br>"
         , "hej <em>världen</em><br>"
         , "<em>hallo</em> welt<br>"
         , "hei verden</code></pre>"
         ])
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
