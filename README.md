These are [js\_of\_ocaml](https://github.com/ocsigen/js_of_ocaml) bindings to [nodejs](https://github.com/nodejs/node)

Get all the power of the amazing `node` ecosystem with the sanity and
type safety of `OCaml`. See examples and a working chat server at the
end.

```shell
$ opam install nodejs
```

# Examples

**Create a file stream, gzip it, write it**

```ocaml
1  let _ =
2    Fs.create_read_stream "code.ml" >|>
3    Zlib.create_gzip () >|>
4    Fs.create_write_stream "NEWCODE_TEST.ml"
```

**Do an HTTP get request**

```ocaml
let () =
  try
    ignore begin

      Nodejs.Https.get api_source begin fun incoming ->
        let collect = ref [] in

        incoming#on_data begin fun b ->
          match b with
          | Nodejs.String s -> collect := s :: !collect
          | Nodejs.Buffer b -> collect := b#to_string () :: !collect

        end;

        incoming#on_end (fun () ->
            print_endline (String.concat "" (List.rev !collect)));

      end
    end
  with Js.Error e -> print_endline (e##.message |> Js.to_string)
```

**Create a site and render directly from jade templates**

```ocaml
open Nodejs

let () =
  let exp = new Express.express in
  let app = new Express.app ~existing:None in

  app#set_app_value (`View_engine "jade");
  app#use (exp#static ".");
  app#get ~path:"/" (fun _ res -> res#render "index.jade");

  app#listen ~port:8080
```

**Create a raw server from the Net module**

```ocaml
let () =
  let server = Net.create_server ~conn_listener:begin fun sock ->
      sock#on_end (fun () -> print_endline "client disconnected");
      sock#write "Hello\r\n";
      sock >|> sock |> ignore
    end ()
  in
  server#listen ~port:8124 begin fun () ->
    let info = server#address in
    print_endline info.Net.address;
    print_endline (info.Net.ip_family |> string_of_ip);
    print_endline (info.Net.port |> string_of_int);
    print_endline "started server"
  end
```

**Typed Decoding of Buffers**

```ocaml
let () =
  let string_decoder = new String_decoder.decoder Utf_8 in
  let cent = new Buffer.buffer (`Array [|0xE2; 0x82; 0xAC|]) in
  (string_decoder#write cent) |> print_endline
```

# Working Chat Server

Working Chat Server
![img](./node_server_working.gif)

Here's the example's source code: which is located along side its
dependencies and make file in the `examples` directory.

**NOTE** You will still need to have npm modules installed, for instance
this example uses `socket.io`, `colors.js` which means you'll need to
have npm installed socket.io and colors at some point. (Although the
`Makefile` in examples will do this npm install for you)

```ocaml
 1  (* Basically a translation of
 2     http://arminboss.de/2013/tutorial-how-to-create-a-basic-chat-with-node-js/ *)
 3  open Nodejs
 4  
 5  let () =
 6    let io = Socket_io.require () in
 7    let server =
 8      Http.create_server begin fun incoming response ->
 9  
10        Fs.read_file ~path:"./client.html" begin fun err data ->
11          response#write_head ~status_code:200 [("Content-type", "text/html")];
12          response#end_ ~data:(String data) ()
13  
14        end
15      end
16    in
17    let app = server#listen ~port:8080 begin fun () ->
18  
19        let s =
20          Printf.sprintf "Started Server and Running node: %s" (new process#version)
21        in
22  
23        Colors_js.colorize ~msg:s ~styles:[Colors_js.Cyan_bg; Colors_js.Inverse] []
24        |> print_endline
25  
26      end
27    in
28  
29    let io = io#listen app in
30    io#sockets#on_connection begin fun socket ->
31  
32      socket#on "message_to_server" begin fun data ->
33  
34        io#sockets#emit
35          ~event_name:"message_to_client"
36          !!(object%js val message = data <!> "message" end)
37  
38      end
39    end
```

The `<!>` infix operator is just a way to get a field of a JavaScript
Object and the `!!` prefix operator is a way lift the js\_of\_ocaml
object literal as a JavaScript object. Notice the high level nature of
the code utilizing OCaml's features like named parameters.

The code assumes that `client.html` is in the same directory, it looks
like this:

```html
 1  <!DOCTYPE html>
 2  <html>
 3  <head>
 4  <script src="https://cdn.socket.io/socket.io-1.3.7.js"></script>
 5  <script type="text/javascript">
 6  var socketio = io.connect("http://localhost:8080");
 7  socketio.on("message_to_client", function(data) {
 8  document.getElementById("chatlog").innerHTML = ("<hr/>" + data['message'] +
 9  document.getElementById("chatlog").innerHTML);
10  });
11  
12  function sendMessage() {
13    var msg = document.getElementById("message_input").value;
14    socketio.emit("message_to_server", { message : msg});
15  }
16  </script>
17  </head>
18  <body>
19          <input type="text" id="message_input"/>
20          <button onclick="sendMessage()">send</button>
21          <div id="chatlog"></div>
22  </body>
23  </html>
```

# Steps to get the example working

I assume that you have `opam` and of course `node`
installed.

1.  Get the `nodejs` package installed on your machine.
    
    ```shell
    $ opam install nodejs colors_js socket_io
    ```

2.  Compile `chat_server.ml` into a working `node` program.
    
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
