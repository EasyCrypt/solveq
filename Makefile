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
.PHONY: all build lib install uninstall examples clean

all: lib
	@true

build: lib

lib:
	$(OCAMLBUILD) libs/solveq.cmo libs/solveq.cmx

main: 
	$(OCAMLBUILD) libs/solveq/Main/main.native

examples: lib
	cd tests && ocaml -rectypes examples.ml monalg_tests.ml

tests: main
	./main.native tests/test_ok.solveq

install: lib
	ocamlfind install solveq META \
	  _build/libs/solveq.cmo \
	  _build/libs/solveq.o \
	  _build/libs/solveq.cmx \
	  _build/libs/solveq.cmi

uninstall:
	ocamlfind remove solveq

reinstall: uninstall install

clean:
	$(OCAMLBUILD) -clean
