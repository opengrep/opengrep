type t = {
  callable_literals : bool;
  send_methods : string list;
  method_object : string option;
  attribute_lookup : string option;
  apply_function : string list;
  symbol_lookup : string list;
}

let none : t =
  {
    callable_literals = false;
    send_methods = [];
    method_object = None;
    attribute_lookup = None;
    apply_function = [];
    symbol_lookup = [];
  }

let python : t = { none with attribute_lookup = Some "getattr" }

let ruby : t =
  {
    none with
    send_methods = [ "send"; "public_send"; "__send__" ];
    method_object = Some "method";
  }

let php : t = { none with callable_literals = true }

let elixir : t =
  { none with apply_function = [ "apply"; "Kernel.apply"; ":erlang.apply" ] }

let julia : t = { none with symbol_lookup = [ "getfield" ] }
let clojure : t = { none with symbol_lookup = [ "resolve"; "ns-resolve" ] }
let r : t = { none with symbol_lookup = [ "get"; "do.call"; "match.fun" ] }

let of_lang (lang : Lang.t) : t =
  match lang with
  | Lang.Python
  | Lang.Python2
  | Lang.Python3 ->
      python
  | Lang.Ruby -> ruby
  | Lang.Php
  | Lang.Hack ->
      php
  | Lang.Elixir -> elixir
  | Lang.Julia -> julia
  | Lang.Clojure -> clojure
  | Lang.R -> r
  | _ -> none

let is_send_method (lang : Lang.t) (name : string) : bool =
  List.exists (String.equal name) (of_lang lang).send_methods
