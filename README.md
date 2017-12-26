<!-- THIS FILE IS GENERATED FROM 'README.src.md'. DO NOT EDIT BY HAND! -->
# pandoc-emphasize-code

*A Pandoc filter for emphasizing code in fenced blocks.*







-   [Usage](#usage)
    -   [Syntax](#syntax)
-   [Rendering to HTML](#rendering-to-html)
    -   [Rendering with LaTeX](#rendering-with-latex)
    -   [Regular Highlighting](#regular-highlighting)
-   [Install](#install)
    -   [From Hackage](#from-hackage)
-   [Build](#build)
-   [Run](#run)
-   [Changelog](#changelog)
-   [License](#license)

Usage
-----

The filter recognizes code blocks with the `emphasize` attribute present:

    ```{.haskell emphasize=2:3-2:14,3:3-3:12}
    myFunc = do
      newStuffHere
      andThisToo notThis
      notSoRelevant
    ```

### Syntax

The value of the `emphasize` attribute is a comma-separated list of ranges. Each range is two positions, separated by a dash. A position is a line number, followed by a colon, ending with a column number. The syntax can be described in EBNF, like so:

``` ebnf
line     = natural number;
column   = natural number;
position = line, ":", column;
range    = position, "-", position;
ranges   = range, { (",", range) };

(* definition of natural number excluded for brevity *)
```

Rendering to HTML
-----------------

The code block above would render the following HTML output:

``` html
<pre class="haskell"><code>myFunc = do
  <em>newStuffHere</em>
  <em>andThisToo</em> notThis
  notSoRelevant</code></pre>
```

When rendering to HTML, the markup can be styled using CSS:

``` css
code em {
  color: red;
  font-style: italic;
}
```

By default, if no custom styling is applied, it will look something like this:

<pre class="haskell"><code>myFunc = do
  <em>newStuffHere</em>
  <em>andThisToo</em> notThis
  notSoRelevant</code></pre>
Note that the there is no additional syntax highlighting when emphasizing code and rendering to HTML, as there is no way to use Pandoc's highlighter and embed custom HTML tags. You might be able to add that using a Javascript highlighter running on the client.

### Rendering with LaTeX

When rendering using LaTeX, two things are required:

-   The `listings` package needs to be included.
-   You need to define a `CodeEmphasis` command, styling the emphasized code in `lstlisting`s.

If you're not using a custom LaTeX template, you can use the YAML front matter in a Markdown source file to add the requirements:

``` yaml
header-includes:
  - \usepackage{listings}
  - \lstset{basicstyle=\ttfamily}
  - \newcommand{\CodeEmphasis}[1]{\textcolor{red}{\textit{#1}}}
```

**NOTE:** When rendering as Beamer slides, any frame including an emphasized block must be marked as `fragile`:

```` markdown
## My Slide {.fragile}

```{.haskell emphasize=2:3-2:14,3:3-3:12}
myFunc = do
  newStuffHere
  andThisToo notThis
  notSoRelevant
```
````

### Regular Highlighting

You can still use regular Pandoc highlighting (the *skylighting* library):

    ``` {.css}
    .hello {
      world: yes;
    }
    ```

It gives you all the nice colors:

``` css
.hello {
  world: yes;
}
```

The drawback is that you have two different highlighting systems now, one for emphasized code, one for regular code blocks.

Install
-------

Executables for Linux and macOS are available in the [Releases page](https://github.com/owickstrom/pandoc-emphasize-code/releases).

### From Hackage

If you'd rather install using `cabal` or `stack`, you can use the following command:

``` sh
cabal install pandoc-emphasize-code
```

The package is [available at Hackage](https://hackage.haskell.org/package/pandoc-emphasize-code).

Build
-----

Requirements:

-   [Cabal](https://www.haskell.org/cabal/) or [Stack](https://docs.haskellstack.org/en/stable/README/), either works.

To install from sources, run:

``` sh
git clone git@github.com:owickstrom/pandoc-emphasize-code.git
cd pandoc-emphasize-code
stack setup
stack install
```

Run
---

If you have installed from sources, and you have `~/.local/bin` on your `PATH`, you can use the filter with Pandoc like so:

``` sh
pandoc --filter pandoc-emphasize-code input.md output.html
```

Changelog
---------

-   **0.1.0**
    -   First release
    -   Support for multiple ranges
    -   Rendering support for HTML, Markdown, and LaTeX

License
-------

[Mozilla Public License Version 2.0](LICENSE)
