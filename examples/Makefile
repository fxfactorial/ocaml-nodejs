pkgs := nodejs,socket_io,express


all:
	ocamlfind ocamlc chat_server.ml -package $(pkgs) -linkpkg
	js_of_ocaml a.out -o chat_server.js

run:all clean
	node chat_server.js

clean:
	rm -f chat_server.cmi chat_server.cmo chat_server.cmt a.out
