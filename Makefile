dist: dist/platform.tar.gz

ARCHIVE_TARGETS = \
  dist/platform/platform \
  dist/platform/completion/bash \
  dist/platform/completion/fish \
  dist/platform/completion/zsh \
  dist/platform/doc/platform.5 \
  dist/platform/doc/platform.8 \
  dist/Makefile

dist/platform.tar.gz: $(ARCHIVE_TARGETS)
	tar -C ./dist -czvf $@ ./platform

SRCS = $(shell \
  find ./src ./app -name '*.hs'; \
  find ./templates -type f; \
  echo platform.cabal \
)

dist/platform/platform: $(SRCS)
	mkdir -p dist/platform
	stack build --pedantic --test --copy-bins --local-bin-path dist/platform

dist/platform/completion/%: dist/platform/platform
	./$< --$(@F)-completion-script platform > dist/platform/completion/$(@F)

PANDOC ?= stack exec pandoc --

dist/platform/doc/%: doc/%.md
	mkdir -p dist/platform
	$(PANDOC) --standalone $< --to man >$@

dist/Makefile: Makefile
	cp $< $@

DESTDIR ?=
PREFIX ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

.PHONY: install
install:
	install -Dm755 platform $(DESTDIR)$(PREFIX)/bin/platform
	install -Dm644 completion/bash $(DESTDIR)$(PREFIX)/share/bash-completion/completions/platform
	install -Dm644 completion/fish $(DESTDIR)$(PREFIX)/share/fish/vendor_completions.d/platform.fish
	install -Dm644 completion/zsh $(DESTDIR)$(PREFIX)/share/zsh/site-functions/_platform
	install -Dm644 doc/platform.5 $(DESTDIR)$(MANPREFIX)/man5/platform.5
	install -Dm644 doc/platform.8 $(DESTDIR)$(MANPREFIX)/man8/platform.8

.PHONY: install.check
install.check: dist/platform.tar.gz
	cp dist/platform.tar.gz /tmp && \
	  cd /tmp && \
	  tar xvf platform.tar.gz && \
	  cd platform && \
	  make install PREFIX=$$HOME/.local
	PATH=$$HOME/.local/bin:$$PATH cram -v integration/tests

# Run this task when you make changes that do impact the templates (and so the
# integration tests that assert no changes will fail). Confirm the changes are
# as expected when deploying.
.PHONY: examples.deploy
examples.deploy:
	for x in example-apps/*; do \
	  (cd "$$x" && AWS_PROFILE=freckle-dev platform deploy --tag example --no-confirm); \
	done
