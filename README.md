These are [js\_of\_ocaml](https://github.com/ocsigen/js_of_ocaml) bindings to [nodejs](https://github.com/nodejs/node)

Get all the power of the amazing `node` ecosystem while still getting
the sanity and type safety of `OCaml`.

The examples directory contains a chat\_server tutorial/example. To
actually have that up and running, you'll need to have `js_of_ocaml`
bindings to [socket.io](https://github.com/socketio/socket.io), those are located [here](https://github.com/fxfactorial/ocaml-npm-socket-io)

# Steps to get the example working

I assume that you have `opam`, `js_of_ocaml` and of course `node`
installed. Until I get this all on `opam` you'll need to do the
following steps.

1.  Get the `nodejs` package installed on your machine.

```shell
$ git clone https://github.com/fxfactorial/ocaml-nodejs
$ cd ocaml-nodejs
$ opam pin add nodejs . -y
```

1.  Get the `socket_io` package installed on your machine.

```shell
$ git clone https://github.com/fxfactorial/ocaml-npm-socket-io
$ cd ocaml-npm-socket-io
$ opam pin add socket_io . -y
```

1.  Compile `chat_server.ml` into a working `node` program

```shell
$ cd examples
$ make
```

and open up localhost:8080, you'll have a working `node` server.

# Issues

1.  `node` has a pretty big API so its going to take me a little bit of
    time to cover the API and the bindings that I'm also writing for
    `express` and `socket.io`
2.  `JavaScript`
