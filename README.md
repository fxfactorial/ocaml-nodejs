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
 3  open Nodejs_kit
 4  
 5  let program () =
 6    let http = Nodejs.Http.require () in
 7    let fs = Nodejs.Fs.require () in
 8    let io = Socket_io.require () in
 9  
10    let port = 8080 in
11    let headers = js_object_of_alist [("Content-Type", "text/html")] in
12    let server =
13      http##createServer
14        !@ begin
15        fun request response ->
16          fs##readFile !$"./client.html"
17            !@ (fun error raw_data ->
18                response##writeHead 200 headers;
19                response##end_data raw_data)
20      end
21    in
22    let app = server##listen port
23        !@ begin fun () ->
24        Printf.sprintf
25          "\n\nStarted Server on local host port %d, node version: %s"
26          port
27          Nodejs.Process.version
28        |> print_endline
29      end
30    in
31    let io = io##listen app in
32    (* Gives back a namespace object *)
33    io##.sockets##on
34      !$ "connection"
35      (* And now we get a socket *)
36      !@ begin fun socket ->
37      socket##on
38        !$ "message_to_server"
39        (* For which we have some data, its an object *)
40        !@ begin fun data ->
41        let innard = Js.Unsafe.get data "message" in
42        io##.sockets##emit
43          !$"message_to_client"
44          (js_object_of_alist [("message", innard)])
45      end
46    end
47  
48  let run p =
49    ignore (p ())
50  
51  let () =
52    run program
```

The `!$` operator is a unary function to turn an OCaml string into a
JavaScript string and the `!@` operator is a unary function to wrap an
OCaml function as a JavaScript callback.

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
