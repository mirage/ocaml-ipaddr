.PHONY: all

all: test_ipaddr.ml
	ocamlfind ocamlc -o test_ipaddr -package oUnit -I ../_build/lib \
		../_build/lib/ipaddr.cmo -linkpkg test_ipaddr.ml
	./test_ipaddr
	@echo
	@rm test_ipaddr.cmi test_ipaddr.cmo
