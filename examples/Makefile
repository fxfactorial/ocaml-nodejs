pkgs := nodejs.high_level,js_of_ocaml.ppx
js_debug := --debug-info --no-inline --pretty

all:buffer file_system high_level events dns net

buffer:buffer_example.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

file_system:file_system.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

high_level:high_level.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

events:events_example.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

dns:dns_example.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

net:net_example.ml
	ocamlfind ocamlc -g -package ${pkgs} -linkpkg $< -o $@
	js_of_ocaml ${js_debug} $@ -o $@.js
	node $@.js

clean:
	@rm -f *.cmi *.cmo *.cmt a.out *.js \
	buffer file_system high_level events \
	dns net

.PHONY:clean file_system buffer dns events net
