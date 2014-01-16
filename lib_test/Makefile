.PHONY: all

FLAGS=-package oUnit -g

all: test_ipaddr.ml test_ipaddr_6.ml test_macaddr.ml
	ocamlfind ocamlc -o test_ipaddr $(FLAGS) -I ../_build/lib \
		../_build/lib/ipaddr.cmo -linkpkg test_ipaddr.ml
	ocamlfind ocamlc -o test_ipaddr6 $(FLAGS) -I ../_build/lib \
		../_build/lib/ipaddr.cmo -linkpkg test_ipaddr_6.ml
	ocamlfind ocamlc -o test_macaddr $(FLAGS) -I ../_build/lib \
		../_build/lib/macaddr.cmo -linkpkg test_macaddr.ml
	./test_ipaddr
	@echo
	./test_ipaddr6
	@echo
	./test_macaddr
	@echo
	@rm test_ipaddr.cmi test_ipaddr.cmo test_ipaddr
	@rm test_ipaddr_6.cmi test_ipaddr_6.cmo test_ipaddr6
	@rm test_macaddr.cmi test_macaddr.cmo test_macaddr
