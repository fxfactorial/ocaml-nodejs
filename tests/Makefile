

all:
	ocamlfind ocamlc -package nodejs exhaust_api.ml -linkpkg
	js_of_ocaml a.out -o exhaust_api.js

clean:
	rm -rf a.out exhaust_api.cmx exhaust_api.cmt \
	exhaust_api.cmo exhaust_api.js exhaust_api.cmi

run:all clean
	node exhaust_api.js
