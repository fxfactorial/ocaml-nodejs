OCaml on nodejs
=================

These are OCaml bindings to Nodejs. I'm currently rewriting everything
from a previous iteration and so things might get outdated quickly.


Included are three levels of abstraction, 

1. `Nodejs`
2. `Nodejs_high_level`
3. `Nodejs_high_level_lwt`

they correspond to the findlib packages: 

```
nodejs
nodejs.high_level
nodejs.high_level_lwt 
```

You will probably be using this library at the 2/3 level.

High level Nodejs API Examples
==================================

All can be compiled and run with these steps: 

```shell
$ ocamlfind ocamlc -g -package nodejs.high_level_lwt -linkpkg code.ml
$ js_of_ocaml --debug-info --no-inline --pretty a.out -o T.js
$ node T.js
```

Using Lwt: 

```ocaml
open Lwt.Infix

let () =
  (Nodejs_high_level_lwt.Fs.read_file (Nodejs_high_level.__filename ())
   >>= fun (err, result) -> result#to_string |> print_endline |> Lwt.return
   >|= fun () -> print_endline "Finished Program and Ordered Async")
  |> Lwt.ignore_result
```

Plain server:

```ocaml
open Nodejs_high_level

let () =
  let s =
    new Net.server (fun client ->
        client#write "Welcome to the Matrix";
        print_endline "Client connected")
  in

  s#listen 8124 (fun () -> print_endline "Created a server")
```
