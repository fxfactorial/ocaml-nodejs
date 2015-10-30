pkgs := nodejs,socket_io
js_debug := --debug-info --no-inline --pretty

# Do not use it breaks the script, very odd!
define colorecho
      @tput setaf 6
      @echo $1
      @tput sgr0
endef

run:all clean
	# @npm install socket.io
	node chat_server.js

all:
	@ocamlfind ocamlc chat_server.ml -package $(pkgs) -linkpkg
	@js_of_ocaml $(js_debug) a.out -o chat_server.js

# clean:
# 	@rm -f chat_server2.cmi chat_server2.cmo chat_server2.cmt a.out

clean:
	@rm -f chat_server.cmi chat_server.cmo chat_server.cmt a.out

.PHONY:clean
