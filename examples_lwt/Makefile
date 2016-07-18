pkgs := nodejs.high_level_lwt
js_debug := --debug-info --no-inline --pretty

file_system:file_system.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

clean:
	@rm -f *.cmi *.cmo *.cmt a.out *.js \
	file_system


.PHONY:clean file_system
