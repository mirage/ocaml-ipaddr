.PHONY: all clean

FLAGS=-package oUnit -package sexplib -g -safe-string -principal

all: test_ipaddr.ml test_macaddr.ml
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
