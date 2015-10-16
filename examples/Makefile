pkgs := nodejs,socket_io

# http://stackoverflow.com/questions/5947742/how-to-change-the-output-color-of-echo-in-linux
# +
# http://eli.thegreenplace.net/2013/12/18/makefile-functions-and-color-output

define colorecho
      @tput setaf 6
      @echo $1
      @tput sgr0
endef

run:all clean
	$(call colorecho, "Running Node with compiled OCaml 🐫  → JavaScript")
	node chat_server.js

all:
	@ocamlfind ocamlc chat_server.ml -package $(pkgs) -linkpkg
	@js_of_ocaml a.out -o chat_server.js

clean:
	@rm -f chat_server.cmi chat_server.cmo chat_server.cmt a.out
