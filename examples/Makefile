pkgs := nodejs,socket_io,express


all:
	ocamlfind ocamlc chat_server.ml -package $(pkgs) -linkpkg
	js_of_ocaml chat_server.byte

run:all
	node chat_server.js
