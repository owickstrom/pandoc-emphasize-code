all: README.md docs/index.html docs/pandoc-emphasize-code.pdf

README.md: README.src.md docs/template.md Makefile
	pandoc \
		-s \
		--toc \
		--template docs/template.md \
		-t gfm \
		--filter pandoc-emphasize-code \
		-o $@ \
		$<

docs/pandoc-emphasize-code.pdf: README.src.md docs/header.tex Makefile
	mkdir -p docs
	pandoc \
		-t latex \
		-H docs/header.tex \
		--toc \
		--filter pandoc-emphasize-code \
		-o $@ \
		$<

docs/index.html: README.src.md Makefile docs/footer.html
	mkdir -p docs
	pandoc \
		-s \
		-t html5 \
		--toc \
		--css 'https://fonts.googleapis.com/css?family=Fira+Sans:400,400i|Fira+Mono:400|Oswald:400,600' \
		--css docs.css \
		--css syntax-highlighting.css \
		--include-after docs/footer.html \
		--filter pandoc-emphasize-code \
		-o $@ \
		$<
