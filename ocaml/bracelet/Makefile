test: bracket.cmo balance.cmo test.cmo
	ocamlc -o $@ $^

test.cmo: bracket.cmo balance.cmo
balance.cmo: bracket.ml balance.ml

%.cmo: %.ml
	ocamlc -c $<

clean:
	rm -f *.cmi *.cmo test
