dist: dist/stackctl.tar.gz

ARCHIVE_TARGETS := \
  dist/stackctl/stackctl \
  dist/stackctl/completion/bash \
  dist/stackctl/completion/fish \
  dist/stackctl/completion/zsh \
  dist/stackctl/doc/stackctl.1 \
  dist/stackctl/doc/stackctl-cat.1 \
  dist/stackctl/doc/stackctl-capture.1 \
  dist/stackctl/doc/stackctl-changes.1 \
  dist/stackctl/doc/stackctl-deploy.1 \
  dist/stackctl/doc/stackctl-version.1 \
  dist/stackctl/Makefile

dist/stackctl.tar.gz: $(ARCHIVE_TARGETS)
	tar -C ./dist -czvf $@ ./stackctl

SRCS := $(shell \
  find ./src ./app -name '*.hs'; \
  echo stackctl.cabal \
)

dist/stackctl/stackctl: $(SRCS)
	mkdir -p ./dist/stackctl
	stack build --pedantic --test --copy-bins --local-bin-path dist/stackctl

dist/stackctl/completion/%: dist/stackctl/stackctl
	mkdir -p ./dist/stackctl/completion
	./$< --$(@F)-completion-script stackctl > dist/stackctl/completion/$(@F)

PANDOC ?= stack exec pandoc --

dist/stackctl/doc/%: doc/%.md
	mkdir -p ./dist/stackctl/doc
	$(PANDOC) --standalone $< --to man >$@

dist/stackctl/Makefile: Makefile
	cp $< $@

.PHONY: clean
clean:
	$(RM) -r ./dist
	stack clean --full

DESTDIR ?=
PREFIX ?= /usr/local
MANPREFIX ?= $(PREFIX)/share/man

.PHONY: install
install:
	install -Dm755 stackctl $(DESTDIR)$(PREFIX)/bin/stackctl
	install -Dm644 completion/bash $(DESTDIR)$(PREFIX)/share/bash-completion/completions/stackctl
	install -Dm644 completion/fish $(DESTDIR)$(PREFIX)/share/fish/vendor_completions.d/stackctl.fish
	install -Dm644 completion/zsh $(DESTDIR)$(PREFIX)/share/zsh/site-functions/_stackctl
	install -Dm644 doc/stackctl.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl.1
	install -Dm644 doc/stackctl-cat.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl-cat.1
	install -Dm644 doc/stackctl-capture.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl-capture.1
	install -Dm644 doc/stackctl-changes.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl-changes.1
	install -Dm644 doc/stackctl-deploy.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl-deploy.1
	install -Dm644 doc/stackctl-version.1 $(DESTDIR)$(MANPREFIX)/man1/stackctl-version.1

.PHONY: uninstall
uninstall:
	$(RM) $(DESTDIR)$(PREFIX)/bin/stackctl
	$(RM) $(DESTDIR)$(PREFIX)/share/bash-completion/completions/stackctl
	$(RM) $(DESTDIR)$(PREFIX)/share/fish/vendor_completions.d/stackctl.fish
	$(RM) $(DESTDIR)$(PREFIX)/share/zsh/site-functions/_stackctl
	$(RM) $(DESTDIR)$(MANPREFIX)/man1/stackctl.1

.PHONY: install.check
install.check: dist/stackctl.tar.gz
	cp dist/stackctl.tar.gz /tmp && \
	  cd /tmp && \
	  tar xvf stackctl.tar.gz && \
	  cd stackctl && \
	  make install PREFIX=$$HOME/.local
	PATH=$$HOME/.local/bin:$$PATH stackctl version
