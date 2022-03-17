PANDOC ?= stack exec pandoc --

dist/stackctl/doc/%: doc/%.md
	mkdir -p dist/stackctl/doc
	$(PANDOC) --standalone $< --to man >$@
