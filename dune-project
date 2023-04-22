(lang dune 3.0)

(name nodejs)

(license BSD-3-clause)

(authors
 "Edgar Aroutiounnian <edgar.factorial@gmail.com>")

(maintainers
 "Edgar Aroutiounnian <edgar.factorial@gmail.com>")

(source
 (github fxfactorial/ocaml-nodejs))

(generate_opam_files true)

(package
 (name nodejs)
 (synopsis
  "js_of_ocaml bindings for nodejs")
 (description
  "Write OCaml, run on node; these are js_of_ocaml bindings to the node
JavaScript API. Get all the power of the node ecosystem with the type
safety of OCaml.")
 (depends
  ocaml
  (js_of_ocaml
   (>= 2.5.2))))
