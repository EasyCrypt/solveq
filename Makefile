OCB_FLAGS   = -use-ocamlfind -I src

OCB = 		ocamlbuild $(OCB_FLAGS)

all : 
	$(OCB) utils.cmo
	$(OCB) groebnerBasis.cmo
	$(OCB) nc_gasbi.cmo

clean:
	$(OCB) -clean



