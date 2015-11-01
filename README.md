These are [js\_of\_ocaml](https://github.com/ocsigen/js_of_ocaml) bindings to [nodejs](https://github.com/nodejs/node)

Get all the power of the amazing `node` ecosystem with the sanity and
type safety of `OCaml`.

```shell
$ opam install nodejs
```

Working Chat Server
![img](./node_server_working.gif)

Here's the example's source code: which is located along side its
dependencies and make file in the `examples` directory.

```ocaml
 1  (* Basically a translation of
 2     http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)
 3  open Nodejs
 4  
 5  let () =
 6    let module H = (val require Http) in
 7    let module Fs = (val require Fs) in
 8    let io = Socket_io.require () in
 9    let server =
10      H.create_server begin fun incoming response ->
11  
12        Fs.read_file ~path:"./client.html" begin fun err data ->
13          response#write_head ~status_code:200 [("Content-type", "text/html")];
14          response#end_ ~data:(H.String data) ()
15  
16        end
17      end
18    in
19    let app = server#listen ~port:8080 begin fun () ->
20        Printf.sprintf
21          "Started Server and Running node: %s" (new Nodejs.process#version)
22        |> print_endline
23      end
24    in
25  
26    let io = io#listen app in
27    io#sockets#on_connection begin fun socket ->
28  
29      socket#on "message_to_server" begin fun data ->
30  
31        io#sockets#emit
32          ~event_name:"message_to_client"
33          !!(object%js val message = data <!> "message" end)
34  
35      end
36    end
```

The `<!>` infix operator is just a way to get a field of a JavaScript
Object and the `!!` prefix operator is a way lift the js\_of\_ocaml
object literal as a JavaScript object. Notice the high level nature of
the code utilizing OCaml's features like named parameters.

# Steps to get the example working

I assume that you have `opam`, `js_of_ocaml` and of course `node`
installed. Until I get this all on `opam` you'll need to do the
following steps.

1.  Get the `nodejs` package installed on your machine.

```shell
$ opam install nodejs
```

1.  Get the `socket_io` package installed on your machine.

```shell
$ git clone https://github.com/fxfactorial/ocaml-npm-socket-io
$ cd ocaml-npm-socket-io
$ opam pin add socket_io . -y
```

1.  Compile `chat_server.ml` into a working `node` program. Note that
    this will install a local node module, the `socket.io` module.

```shell
$ cd examples
$ make
```

and open up localhost:8080, you'll have a working `node` server.

(Note that you'll only need to call `make` once, afterwards you can
directly just invoke node with `node chat_server.js`.)

# Issues

1.  `node` has a pretty big API so its going to take me a little bit of
    time to cover the API and the bindings that I'm also writing for
    `express` and `socket.io`
2.  `JavaScript`
