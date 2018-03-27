# -*- Makefile -*-

# --------------------------------------------------------------------
OCAMLBUILD_JOBS  ?= 1
OCAMLBUILD_BIN   ?= ocamlbuild
OCAMLBUILD_EXTRA ?= 
OCAMLBUILD_OPTS  := -use-ocamlfind -j $(OCAMLBUILD_JOBS)

# In Emacs, use classic display to enable error jumping.
ifeq ($(shell echo $$TERM), dumb)
OCAMLBUILD_OPTS += -classic-display
endif

OCAMLBUILD_OPTS += $(OCAMLBUILD_EXTRA)
OCAMLBUILD      := $(OCAMLBUILD_BIN) $(OCAMLBUILD_OPTS)

# --------------------------------------------------------------------
MAIN := libsolveq

# --------------------------------------------------------------------
.PHONY: all build native byte install uninstall examples clean

all: build
	@true

build: native

byte:
	$(OCAMLBUILD) $(MAIN).cma

native:
	$(OCAMLBUILD) $(MAIN).cmxa

examples: byte
	cd tests && ocaml -rectypes examples.ml

install: native
	@true

uninstall:
	@true

clean:
	$(OCAMLBUILD) -clean
	rm -f $(MAIN).cma $(MAIN).cmxa
