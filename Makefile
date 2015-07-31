native:
	ocamlbuild um.native

test: native
	./um.native < sandmark.umz > sandmark.output
	diff sandmark.output sandmark-output.txt > sandmark.diff

clean:
	ocamlbuild -clean
	rm -f sandmark.diff
	rm -f sandmark.output

.PHONY: native
