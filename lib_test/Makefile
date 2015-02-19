.PHONY: all clean

OCAML_VERSION=$(shell ocaml -version | sed -n "s/.*version \(.*\)$$/\1/p")

ifneq      (,$(findstring 3.12.,$(OCAML_VERSION)))
SAFE_STRING=
else ifneq (,$(findstring 4.00.,$(OCAML_VERSION)))
SAFE_STRING=
else ifneq (,$(findstring 4.01.,$(OCAML_VERSION)))
SAFE_STRING=
else
SAFE_STRING=-safe-string
endif

FLAGS=-package oUnit,sexplib,bytes -g $(SAFE_STRING) -principal

all: test_ipaddr.ml test_macaddr.ml
	@echo Detected OCaml $(OCAML_VERSION)
	ocamlfind ocamlc -o test_ipaddr $(FLAGS) -I ../_build/lib \
		../_build/lib/macaddr.cmo ../_build/lib/ipaddr.cmo \
		-linkpkg test_ipaddr.ml
	ocamlfind ocamlc -o test_macaddr $(FLAGS) -I ../_build/lib \
		../_build/lib/macaddr.cmo -linkpkg test_macaddr.ml
	./test_ipaddr
	@echo
	./test_macaddr
	@echo
	$(MAKE) clean

clean:
	rm -f test_ipaddr.cmi test_ipaddr.cmo test_ipaddr
	rm -f test_macaddr.cmi test_macaddr.cmo test_macaddr
