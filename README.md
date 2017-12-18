# pandoc-emphasize-code

_A Pandoc filter for emphasizing code in fenced blocks._

## Usage

The filter recognizes code blocks with the `emphasize` attribute present.

    ```{emphasize=2:3-3:13}
    myFunc = do
      newStuffHere
      andThisToo notThis
      notSoRelevant
    ```

Giving HTML output:

    <pre><code>myFunc = do
      <em>newStuffHere
      andThisToo</em> notThis
      notSoRelevant
    </code></pre>

Which can be styled with bold font or colors.

<pre><code>myFunc = do
  <b>newStuffHere
  andThisToo</b> notThis
  notSoRelevant
</code></pre>

## Install

Executables for Linux and macOS are available in the [Releases
page](https://github.com/owickstrom/pandoc-emphasize-code/releases).

### From Hackage

If you'd rather install using `cabal` or `stack`, you can use the following
command:

```bash
cabal install pandoc-emphasize-code
```

The package is [available at Hackage](https://hackage.haskell.org/package/pandoc-emphasize-code).

## Build

Requirements:

* [Cabal](https://www.haskell.org/cabal/) or
  [Stack](https://docs.haskellstack.org/en/stable/README/), either works.

To install from sources, run:

```bash
git clone git@github.com:owickstrom/pandoc-emphasize-code.git
cd pandoc-emphasize-code
cabal configure
cabal install
```

## Run

If you have installed from sources, and you have `~/.local/bin` on your
`PATH`, you can use the filter with Pandoc like so:

```bash
pandoc --filter pandoc-emphasize-code input.md output.html
```

## Changelog

[CHANGELOG.md](CHANGELOG.md)

## License

[Mozilla Public License Version 2.0](LICENSE)
