pkgs := nodejs,socket_io

# Do not use it breaks the script, very odd!
define colorecho
      @tput setaf 6
      @echo $1
      @tput sgr0
endef

run:all clean
	@echo "Running Node server, check localhost:8080\n"
	@npm install socket.io
	node chat_server.js

all:
	@ocamlfind ocamlc chat_server.ml -package $(pkgs) -linkpkg
	@js_of_ocaml a.out -o chat_server.js

clean:
	@rm -f chat_server.cmi chat_server.cmo chat_server.cmt a.out
