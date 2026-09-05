(* Per-language taint/call-graph settings (HOF configs, constructor patterns). *)

type hof_kind =
  | MethodHOF of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
    }
  | FunctionHOF of {
      functions : string list;
      arity : int;
      callback_index : int;
      data_index : int;
      taint_arg_index : int;
    }
  | ReturningFunctionHOF of {
      methods : string list;
    }

type collection_model_kind =
  | ArgTaintsThis of {
      methods : string list;
      arity : int;
      taint_arg_index : int;
      returns_this : bool;
    }
  | ThisTaintsReturn of {
      methods : string list;
      arity : int;
    }

type t = {
  hof_configs : hof_kind list;
  collection_configs : collection_model_kind list;
  constructor_names : string list;
  uses_new_keyword : bool;
  (* Methods invoking `self` as a function (Runnable.run, Proc#call): a Fun-shaped receiver call becomes a direct lambda invocation. *)
  invoke_methods : string list;
  (* Callee leaf names short-circuited to [None] in [identify_callee]. *)
  (* [true] makes [extract_calls] skip nested fdefs/lambdas; unsafe where they need the enclosing scope ([self] in Python methods). *)
  skip_nested_in_extract_calls : bool;
}

let empty = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let python = {
  hof_configs = [
    FunctionHOF { functions = ["map"; "filter"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0 };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["append"; "add"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgTaintsThis { methods = ["extend"; "update"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ThisTaintsReturn { methods = ["pop"]; arity = 0 };
    ThisTaintsReturn { methods = ["get"; "pop"; "setdefault"]; arity = 1 };
    ThisTaintsReturn { methods = ["get"; "pop"; "setdefault"]; arity = 2 };
    ThisTaintsReturn { methods = ["copy"; "keys"; "values"; "items"]; arity = 0 };
  ];
  constructor_names = ["__init__"];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let ruby = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "each"; "select"; "filter"; "flat_map"; "collect"; "find"; "detect"];
      arity = 1;
      taint_arg_index = 0;
    };
    ReturningFunctionHOF {
      methods = ["map"; "each"; "select"; "filter"; "flat_map"; "collect"; "find"; "detect"];
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["push"; "append"; "unshift"; "prepend"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgTaintsThis { methods = ["merge!"; "update"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ThisTaintsReturn { methods = ["pop"; "shift"; "first"; "last"]; arity = 0 };
    ThisTaintsReturn { methods = ["fetch"; "dig"; "slice"]; arity = 1 };
    ThisTaintsReturn { methods = ["fetch"; "dig"]; arity = 2 };
    ThisTaintsReturn { methods = ["to_s"; "join"; "flatten"]; arity = 0 };
    ThisTaintsReturn { methods = ["join"]; arity = 1 };
  ];
  constructor_names = ["initialize"];
  uses_new_keyword = false;
  invoke_methods = ["call"];
  (* Safe: RSpec specs are anonymous-lambda nests with no [self.X] inheritance. *)
  skip_nested_in_extract_calls = true;
}

let crystal = ruby

let javascript = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "flatMap"; "filter"; "forEach"; "find"; "findIndex"; "some"; "every"];
      arity = 1;
      taint_arg_index = 0;
    };
    MethodHOF { methods = ["reduce"; "reduceRight"]; arity = 2; taint_arg_index = 1 };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["set"]; arity = 2; taint_arg_index = 1; returns_this = true };
    ArgTaintsThis { methods = ["push"; "unshift"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ThisTaintsReturn { methods = ["get"]; arity = 1 };
    ThisTaintsReturn { methods = ["pop"; "shift"]; arity = 0 };
    ThisTaintsReturn { methods = ["at"]; arity = 1 };
    ThisTaintsReturn { methods = ["toString"; "valueOf"; "join"]; arity = 0 };
    ThisTaintsReturn { methods = ["join"]; arity = 1 };
  ];
  constructor_names = ["constructor"];
  uses_new_keyword = true;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let typescript = {
  javascript with
  hof_configs = javascript.hof_configs;
}

let java = {
  hof_configs = [
    MethodHOF { methods = ["map"; "filter"; "forEach"; "flatMap"]; arity = 1; taint_arg_index = 0 };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgTaintsThis { methods = ["add"; "addFirst"; "addLast"; "push"; "offer"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["add"; "set"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgTaintsThis { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgTaintsThis { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = true };
    ThisTaintsReturn { methods = ["get"; "getFirst"; "getLast"; "peek"; "poll"; "pop"; "remove"]; arity = 1 };
    ThisTaintsReturn { methods = ["toString"; "getFirst"; "getLast"; "peek"; "poll"; "pop"]; arity = 0 };
    ThisTaintsReturn { methods = ["next"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  uses_new_keyword = true;
  invoke_methods = ["run"; "call"; "apply"; "accept"; "invoke"];
  skip_nested_in_extract_calls = false;
}

let kotlin = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all"];
      arity = 0;
      taint_arg_index = 0;
    };
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "find"; "any"; "all"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["add"; "addFirst"; "addLast"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["put"; "putIfAbsent"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgTaintsThis { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ThisTaintsReturn { methods = ["get"; "getOrNull"]; arity = 1 };
    ThisTaintsReturn { methods = ["getOrDefault"]; arity = 2 };
    ThisTaintsReturn { methods = ["first"; "last"; "removeFirst"; "removeLast"]; arity = 0 };
    ThisTaintsReturn { methods = ["toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"; "init"; "constructor"];
  uses_new_keyword = false;
  invoke_methods = ["invoke"];
  skip_nested_in_extract_calls = false;
}

let scala = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "foreach"; "flatMap"; "find"; "exists"; "forall"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["append"; "prepend"; "addOne"; "add"]; arity = 1; taint_arg_index = 0; returns_this = true };
    ArgTaintsThis { methods = ["put"; "update"; "addOne"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ThisTaintsReturn { methods = ["head"; "last"]; arity = 0 };
    ThisTaintsReturn { methods = ["apply"; "get"; "getOrElse"]; arity = 1 };
    ThisTaintsReturn { methods = ["mkString"; "toString"]; arity = 0 };
  ];
  constructor_names = ["<init>"];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let csharp = {
  hof_configs = [
    MethodHOF {
      methods = ["Select"; "Where"; "ForEach"; "SelectMany"; "First"; "Any"; "All"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["Add"; "Push"; "Enqueue"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["Insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ArgTaintsThis { methods = ["Add"; "TryAdd"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ThisTaintsReturn { methods = ["Pop"; "Dequeue"; "Peek"]; arity = 0 };
    ThisTaintsReturn { methods = ["ElementAt"; "GetValueOrDefault"]; arity = 1 };
    ThisTaintsReturn { methods = ["ToString"]; arity = 0 };
  ];
  constructor_names = [".ctor"];
  uses_new_keyword = true;
  invoke_methods = ["Invoke"];
  skip_nested_in_extract_calls = false;
}

let go = {
  (* No leaf-name HOF configs: bare leaf names would match unrelated calls corpus-wide; auto-detection handles function-ref args. *)
  hof_configs = [];
  collection_configs = [
    ArgTaintsThis { methods = ["Store"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ThisTaintsReturn { methods = ["Load"]; arity = 1 };
  ];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let rust = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "for_each"; "filter"; "flat_map"; "find"; "any"; "all"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["push"; "push_front"; "push_back"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["insert"]; arity = 2; taint_arg_index = 1; returns_this = false };
    ThisTaintsReturn { methods = ["pop"; "pop_front"; "pop_back"]; arity = 0 };
    ThisTaintsReturn { methods = ["get"; "get_mut"; "remove"]; arity = 1 };
    ThisTaintsReturn { methods = ["into_iter"; "iter"; "iter_mut"]; arity = 0 };
  ];
  constructor_names = ["new"];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let swift = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "filter"; "forEach"; "flatMap"; "compactMap"; "first"; "contains"];
      arity = 1;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [
    ArgTaintsThis { methods = ["append"]; arity = 1; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["insert"]; arity = 2; taint_arg_index = 0; returns_this = false };
    ArgTaintsThis { methods = ["updateValue"]; arity = 2; taint_arg_index = 0; returns_this = false };
    ThisTaintsReturn { methods = ["popLast"; "removeFirst"; "removeLast"; "first"; "last"]; arity = 0 };
    ThisTaintsReturn { methods = ["remove"]; arity = 1 };
  ];
  constructor_names = ["init"];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let php = {
  hof_configs = [
    FunctionHOF { functions = ["array_map"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0 };
    FunctionHOF { functions = ["array_filter"; "array_walk"]; arity = 2; callback_index = 1; data_index = 0; taint_arg_index = 0 };
  ];
  collection_configs = [];
  constructor_names = ["__construct"];
  uses_new_keyword = true;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let cpp = {
  hof_configs = [
    FunctionHOF { functions = ["for_each"]; arity = 3; callback_index = 2; data_index = 0; taint_arg_index = 0 };
    FunctionHOF { functions = ["transform"]; arity = 4; callback_index = 3; data_index = 0; taint_arg_index = 0 };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let c = {
  cpp with
  hof_configs = cpp.hof_configs;
}

let ocaml_lang = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let lua = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let dart = {
  hof_configs = [
    MethodHOF {
      methods = ["map"; "where"; "forEach"; "expand"; "firstWhere";
                 "lastWhere"; "any"; "every"; "removeWhere"; "retainWhere"];
      arity = 1;
      taint_arg_index = 0;
    };
    (* reduce(combine) - combine(value, element), the element (arg 1) comes
       from the collection *)
    MethodHOF { methods = ["reduce"]; arity = 1; taint_arg_index = 1 };
  ];
  collection_configs = [
    (* List.add, Set.add, List.addAll, Map.addEntries - item taints this *)
    ArgTaintsThis { methods = ["add"; "addAll"; "addEntries"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* List.insert(index, item) - item taints this *)
    ArgTaintsThis { methods = ["insert"; "insertAll"]; arity = 2; taint_arg_index = 1; returns_this = false };
    (* StringBuffer.write/writeln/writeAll - str taints this *)
    ArgTaintsThis { methods = ["write"; "writeln"; "writeAll"]; arity = 1; taint_arg_index = 0; returns_this = false };
    (* accessors - this taints return *)
    ThisTaintsReturn { methods = ["removeLast"; "toString"; "join"; "toList"; "toSet"]; arity = 0 };
    ThisTaintsReturn { methods = ["removeAt"; "elementAt"; "remove"; "join"]; arity = 1 };
  ];
  (* Dart constructors are class-named (User.User), which is_constructor
     covers via the class-name equality check *)
  constructor_names = [];
  uses_new_keyword = false;
  (* Function objects: f.call(args) invokes the closure f *)
  invoke_methods = ["call"];
  skip_nested_in_extract_calls = false;
}

let elixir = {
  hof_configs = [
    FunctionHOF {
      functions = ["Enum.map"; "Enum.each"; "Enum.filter"; "Enum.flat_map"; "Enum.find"];
      arity = 2;
      callback_index = 1;
      data_index = 0;
      taint_arg_index = 0;
    };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let julia = {
  hof_configs = [
    FunctionHOF { functions = ["map"; "foreach"; "filter"]; arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0 };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let clojure = {
  hof_configs = [
    FunctionHOF {
      functions = ["map"; "filter"; "keep"; "remove"; "some"; "every?";
                   "mapv"; "filterv"; "mapcat"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 0;
    };
    FunctionHOF {
      functions = ["reduce"];
      arity = 3; callback_index = 0; data_index = 2; taint_arg_index = 1;
    };
    FunctionHOF {
      functions = ["reduce"];
      arity = 2; callback_index = 0; data_index = 1; taint_arg_index = 1;
    };
  ];
  collection_configs = [];
  constructor_names = [];
  uses_new_keyword = false;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let apex = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["<init>"];
  uses_new_keyword = true;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let vb = {
  hof_configs = [];
  collection_configs = [];
  constructor_names = ["New"];
  uses_new_keyword = true;
  invoke_methods = [];
  skip_nested_in_extract_calls = false;
}

let get (lang : Lang.t) : t =
  match lang with
  | Lang.Python | Lang.Python2 | Lang.Python3 -> python
  | Lang.Ruby -> ruby
  | Lang.Crystal -> crystal
  | Lang.Js -> javascript
  | Lang.Ts -> typescript
  | Lang.Java -> java
  | Lang.Kotlin -> kotlin
  | Lang.Scala -> scala
  | Lang.Csharp -> csharp
  | Lang.Go -> go
  | Lang.Rust -> rust
  | Lang.Swift -> swift
  | Lang.Php -> php
  | Lang.Cpp -> cpp
  | Lang.C -> c
  | Lang.Ocaml -> ocaml_lang
  | Lang.Lua -> lua
  | Lang.Dart -> dart
  | Lang.Elixir -> elixir
  | Lang.Julia -> julia
  | Lang.Clojure -> clojure
  | Lang.Apex -> apex
  | Lang.Vb -> vb
  | _ -> empty

(* Languages where one scope holds several concrete functions of one name
   and arity told apart by parameter types. Elsewhere such definitions are
   pattern clauses (Elixir, Clojure) or redefinitions, never a group. *)
let overloads_by_type (lang : Lang.t) : bool =
  match lang with
  | Lang.Java
  | Lang.Kotlin
  | Lang.Scala
  | Lang.Csharp
  | Lang.Swift
  | Lang.Cpp
  | Lang.Dart
  | Lang.Apex ->
      true
  | _ -> false

let hof_method_names (lang : Lang.t) : string list =
  (get lang).hof_configs |> List.concat_map (function
    | MethodHOF { methods; _ }
    | ReturningFunctionHOF { methods; _ } -> methods
    | FunctionHOF _ -> [])

let hof_function_specs (lang : Lang.t) : (string list * int) list =
  (get lang).hof_configs |> List.filter_map (function
    | FunctionHOF { functions; callback_index; _ } ->
      Some (functions, callback_index)
    | MethodHOF _ | ReturningFunctionHOF _ -> None)
