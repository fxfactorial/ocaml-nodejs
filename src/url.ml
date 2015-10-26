open Nodejs_kit

(* Not sure if this is all correct *)
type url_parsed = <protocol : js_str Js.opt;
                   slashes : bool;
                   auth : js_str Js.opt;
                   host : js_str Js.opt;
                   port : int Js.opt;
                   hostname : js_str;
                   hash : js_str Js.opt;
                   search : js_str Js.opt;
                   query : js_str Js.opt;
                   pathname : js_str;
                   path : js_str;
                   href : js_str > Js.t

class type url = object

  method parse : js_str -> 'a Js.t Js.meth

  method parse_parseQueryString : js_str -> bool -> url_parsed Js.meth

  method parse_parseQueryStringSlashesDenoteHost :
    js_str -> bool -> bool -> 'a Js.t Js.meth

  method format : url_parsed -> js_str Js.meth

  method resolve : js_str -> js_str -> js_str Js.meth

end

let require () : url Js.t = require "url"
