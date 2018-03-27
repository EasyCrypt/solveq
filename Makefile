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
	ocamlfind install libsolveq META \
	  _build/src/$(MAIN).cma \
	  _build/src/$(MAIN).cmxa \
	  _build/src/$(MAIN).a \
	  _build/src/*.cmi \
	  _build/src/*.cmx

uninstall:
	ocamlfind remove libsolveq

clean:
	$(OCAMLBUILD) -clean
	rm -f $(MAIN).cma $(MAIN).cmxa
